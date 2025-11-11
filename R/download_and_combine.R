# Workflow to download, validate, and combine data from multiple URLs
library(dplyr)
library(pdftools)
library(stringr)

# Source the extraction function
source('R/read_file.R')

cat("=== Download, Validate, and Combine Workflow ===\n\n")

# ============================================================================
# STEP 1: Define URLs and download PDFs
# ============================================================================

# Define your PDF URLs here
pdf_urls <- c(
    "https://example.com/competition1.pdf",
    "https://example.com/competition2.pdf",
    "https://example.com/competition3.pdf"
)

# Create a temporary download directory
download_dir <- "data/downloads"
if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
    cat("✓ Created download directory:", download_dir, "\n")
}

cat("STEP 1: Downloading PDFs from URLs\n")
cat("===================================\n")

downloaded_files <- character()

for (i in seq_along(pdf_urls)) {
    url <- pdf_urls[i]
    # Extract filename from URL or create a numbered name
    filename <- basename(url)
    if (filename == "" || !grepl("\\.pdf$", filename, ignore.case = TRUE)) {
        filename <- paste0("downloaded_", i, ".pdf")
    }

    filepath <- file.path(download_dir, filename)

    cat("Downloading:", url, "\n")
    cat("  -> Saving to:", filepath, "\n")

    tryCatch({
        download.file(url, filepath, mode = "wb", quiet = TRUE)
        downloaded_files <- c(downloaded_files, filepath)
        cat("  ✓ Success\n\n")
    }, error = function(e) {
        cat("  ✗ Error:", e$message, "\n\n")
    })
}

cat("Downloaded", length(downloaded_files), "of", length(pdf_urls), "files\n\n")

# ============================================================================
# STEP 2: Extract data from downloaded PDFs
# ============================================================================

cat("STEP 2: Extracting data from downloaded PDFs\n")
cat("=============================================\n")

if (length(downloaded_files) == 0) {
    cat("✗ No files to process. Exiting.\n")
    quit(save = "no", status = 1)
}

# Extract scores from all downloaded PDFs
new_scores_list <- list()

for (pdf_file in downloaded_files) {
    cat("Processing:", basename(pdf_file), "\n")
    tryCatch({
        scores <- extract_scores_labeled(pdf_file)
        if (nrow(scores) > 0) {
            new_scores_list[[pdf_file]] <- scores
            cat("  ✓ Extracted", nrow(scores), "school records\n")
        } else {
            cat("  ⚠ No schools found in this file\n")
        }
    }, error = function(e) {
        cat("  ✗ Error extracting:", e$message, "\n")
    })
}

# Combine all new scores
if (length(new_scores_list) == 0) {
    cat("\n✗ No data extracted from any files. Exiting.\n")
    quit(save = "no", status = 1)
}

new_scores_df <- bind_rows(new_scores_list)

# Calculate General_Effect_Total (sum of three effect scores) and Music_Eff_Avg_Total
new_scores_df <- new_scores_df %>%
    mutate(
        General_Effect_Total = Music_Eff1_Tot + Music_Eff2_Tot + Visual_Eff_Tot,
        Music_Eff_Avg_Total = (Music_Eff1_Tot + Music_Eff2_Tot) / 2
    )

cat("\n✓ Total new records extracted:", nrow(new_scores_df), "\n")
cat("✓ Calculated General_Effect_Total and Music_Eff_Avg_Total for all records\n\n")

# ============================================================================
# STEP 3: Validate the new data
# ============================================================================

cat("STEP 3: Validating new data\n")
cat("============================\n")

# Define validation rules
component_scores_tot <- c("Music_Ind_Tot", "Music_Ens_Tot",
                          "Visual_Ind_Tot", "Visual_Ens_Tot",
                          "Music_Eff1_Tot", "Music_Eff2_Tot", "Visual_Eff_Tot")

component_scores_total <- c("Music_Total", "Visual_Total")

all_validated_fields <- c(component_scores_tot, component_scores_total)

# Check for invalid records in new data
new_invalid <- new_scores_df %>%
    filter(
        # Check 0-20 range fields
        if_any(all_of(all_validated_fields), ~ !is.na(.) & (. < 0 | . > 20)) |
        # Check Timing_Penalty_Tot (must be <= 0)
        (!is.na(Timing_Penalty_Tot) & (Timing_Penalty_Tot < -10 | Timing_Penalty_Tot > 0))
    )

cat("New records:", nrow(new_scores_df), "\n")
cat("Invalid records:", nrow(new_invalid), "\n")
cat("Valid records:", nrow(new_scores_df) - nrow(new_invalid), "\n")

if (nrow(new_invalid) > 0) {
    cat("\n⚠ WARNING: Found invalid records in new data:\n")
    invalid_summary <- new_invalid %>%
        select(School, Competition_Name, Event_Date) %>%
        distinct() %>%
        arrange(School)
    print(invalid_summary, n = Inf)

    cat("\nDo you want to:\n")
    cat("  1. Include all records (including invalid)\n")
    cat("  2. Exclude invalid records\n")
    cat("  3. Stop and review manually\n")
    cat("\nFor automated processing, we'll exclude invalid records.\n")

    # Filter to valid records only
    new_scores_df_valid <- new_scores_df %>%
        filter(
            # All 0-20 range fields must be valid
            if_all(all_of(all_validated_fields), ~ is.na(.) | (. >= 0 & . <= 20)) &
            # Timing_Penalty_Tot must be between -10 and 0
            (is.na(Timing_Penalty_Tot) | (Timing_Penalty_Tot >= -10 & Timing_Penalty_Tot <= 0))
        )

    cat("✓ Proceeding with", nrow(new_scores_df_valid), "valid records\n\n")
} else {
    new_scores_df_valid <- new_scores_df
    cat("✓ All new records are valid\n\n")
}

# ============================================================================
# STEP 4: Combine with existing data
# ============================================================================

cat("STEP 4: Combining with existing data\n")
cat("=====================================\n")

# Load existing data (scores_labeled_df is already loaded from read_file.R)
cat("Existing records:", nrow(scores_labeled_df), "\n")
cat("New valid records:", nrow(new_scores_df_valid), "\n")

# Check for duplicates
duplicate_check <- new_scores_df_valid %>%
    inner_join(
        scores_labeled_df,
        by = c("School", "Competition_Name", "Event_Date")
    )

if (nrow(duplicate_check) > 0) {
    cat("\n⚠ WARNING: Found", nrow(duplicate_check), "potential duplicate records:\n")
    dup_summary <- duplicate_check %>%
        select(School, Competition_Name, Event_Date) %>%
        distinct()
    print(dup_summary, n = 20)
    cat("\nDuplicates will be kept (you can remove them later if needed)\n")
}

# Combine datasets
combined_scores_df <- bind_rows(scores_labeled_df, new_scores_df_valid)

cat("\n✓ Combined dataset created\n")
cat("  Total records:", nrow(combined_scores_df), "\n")
cat("  Total schools:", n_distinct(combined_scores_df$School), "\n")
cat("  Total competitions:", n_distinct(combined_scores_df$Competition_Name), "\n")

# ============================================================================
# STEP 5: Save results
# ============================================================================

cat("\nSTEP 5: Saving results\n")
cat("======================\n")

# Save combined dataset
output_file <- "data/combined_scores.csv"
write.csv(combined_scores_df, output_file, row.names = FALSE)
cat("✓ Combined data saved to:", output_file, "\n")

# Save validation report
validation_file <- "data/download_validation_report.txt"
sink(validation_file)
cat("=== Download and Validation Report ===\n")
cat("Generated:", Sys.time(), "\n\n")
cat("Downloaded URLs:\n")
for (i in seq_along(pdf_urls)) {
    cat("  ", i, ".", pdf_urls[i], "\n")
}
cat("\nExtraction Summary:\n")
cat("  Files processed:", length(downloaded_files), "\n")
cat("  Records extracted:", nrow(new_scores_df), "\n")
cat("  Valid records:", nrow(new_scores_df_valid), "\n")
cat("  Invalid records:", nrow(new_invalid), "\n")
cat("\nCombined Dataset:\n")
cat("  Total records:", nrow(combined_scores_df), "\n")
cat("  Total schools:", n_distinct(combined_scores_df$School), "\n")
cat("  Total competitions:", n_distinct(combined_scores_df$Competition_Name), "\n")
sink()
cat("✓ Validation report saved to:", validation_file, "\n")

cat("\n=== WORKFLOW COMPLETE ===\n")
cat("\nNext steps:\n")
cat("  1. Review validation report:", validation_file, "\n")
cat("  2. Use combined dataset: combined_scores_df\n")
cat("  3. Run correlation analysis or other visualizations\n")
