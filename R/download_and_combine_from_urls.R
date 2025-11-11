# Workflow to download PDFs from base_url pages, validate, and combine data
library(dplyr)
library(rvest)
library(httr)
library(stringr)
library(purrr)
library(pdftools)

# Source the extraction function
source('R/read_file.R')

cat("=== Download from Base URLs, Validate, and Combine Workflow ===\n\n")

# ============================================================================
# STEP 1: Define base URLs and scrape PDF links
# ============================================================================

# Define your base URLs here (pages that list PDF files)
base_urls <- c(
    "https://www.coloradomarching.org/scores",
    "https://www.coloradomarching.org/copy-of-scores",
    "https://www.coloradomarching.org/2023-recaps",
    "https://www.coloradomarching.org/2022-recaps",
    "https://www.coloradomarching.org/2021-recaps"
    # Add more URLs as needed:
    # "https://www.coloradomarching.org/2023-scores",
    # "https://www.coloradomarching.org/2022-scores"
)

cat("STEP 1: Scraping PDF links from base URLs\n")
cat("==========================================\n")

all_pdf_links <- character()

for (base_url in base_urls) {
    cat("\nScraping:", base_url, "\n")

    tryCatch({
        # Read the page
        page <- read_html(base_url)

        # Extract all links that end in ".pdf"
        pdf_links <- page %>%
            html_nodes("a") %>%
            html_attr("href") %>%
            na.omit() %>%
            # Ensure full URLs
            map_chr(~ ifelse(str_starts(.x, "http"), .x, url_absolute(.x, base_url))) %>%
            # Filter for pdf files
            keep(~ str_detect(.x, "(?i)\\.pdf$"))

        cat("  ✓ Found", length(pdf_links), "PDF links\n")
        all_pdf_links <- c(all_pdf_links, pdf_links)

    }, error = function(e) {
        cat("  ✗ Error scraping page:", e$message, "\n")
    })
}

# Remove duplicates
all_pdf_links <- unique(all_pdf_links)

cat("\n✓ Total unique PDF links found:", length(all_pdf_links), "\n")

if (length(all_pdf_links) == 0) {
    cat("✗ No PDF links found. Exiting.\n")
    quit(save = "no", status = 1)
}

# ============================================================================
# STEP 2: Download PDFs
# ============================================================================

cat("\nSTEP 2: Downloading PDFs\n")
cat("========================\n")

# Create download directory
download_dir <- "data/downloads"
if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
    cat("✓ Created download directory:", download_dir, "\n")
}

downloaded_files <- character()
skipped_files <- 0
failed_downloads <- 0

for (link in all_pdf_links) {
    filename <- basename(link)
    # Clean filename if needed
    filename <- URLdecode(filename)
    filepath <- file.path(download_dir, filename)

    if (file.exists(filepath)) {
        cat("  ○", filename, "already exists — skipping\n")
        downloaded_files <- c(downloaded_files, filepath)
        skipped_files <- skipped_files + 1
    } else {
        cat("  ↓ Downloading:", filename, "\n")
        resp <- try(GET(link, write_disk(filepath, overwrite = FALSE), timeout(30)), silent = TRUE)

        if (!inherits(resp, "try-error") && resp$status_code == 200) {
            downloaded_files <- c(downloaded_files, filepath)
            cat("    ✓ Success\n")
        } else {
            status <- ifelse(inherits(resp, "try-error"), "error", paste("status:", resp$status_code))
            cat("    ✗ Failed (", status, ")\n", sep = "")
            failed_downloads <- failed_downloads + 1
        }
    }
}

cat("\n✓ Download Summary:\n")
cat("  Total links:", length(all_pdf_links), "\n")
cat("  Successfully downloaded:", length(downloaded_files) - skipped_files, "\n")
cat("  Already existed:", skipped_files, "\n")
cat("  Failed:", failed_downloads, "\n")
cat("  Total available for processing:", length(downloaded_files), "\n\n")

# ============================================================================
# STEP 3: Extract data from downloaded PDFs
# ============================================================================

cat("STEP 3: Extracting data from PDFs\n")
cat("==================================\n")

if (length(downloaded_files) == 0) {
    cat("✗ No files to process. Exiting.\n")
    quit(save = "no", status = 1)
}

# Extract scores from all downloaded PDFs
new_scores_list <- list()
extraction_errors <- 0

for (pdf_file in downloaded_files) {
    cat("Processing:", basename(pdf_file), "\n")
    tryCatch({
        # Pass just the filename and the download directory
        scores <- extract_scores_labeled(basename(pdf_file), directory = download_dir)
        if (nrow(scores) > 0) {
            new_scores_list[[pdf_file]] <- scores
            cat("  ✓ Extracted", nrow(scores), "school records\n")
        } else {
            cat("  ⚠ No schools found in this file\n")
        }
    }, error = function(e) {
        cat("  ✗ Error extracting:", e$message, "\n")
        extraction_errors <<- extraction_errors + 1
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

cat("\n✓ Extraction Summary:\n")
cat("  Files processed:", length(downloaded_files), "\n")
cat("  Files with data:", length(new_scores_list), "\n")
cat("  Extraction errors:", extraction_errors, "\n")
cat("  Total records extracted:", nrow(new_scores_df), "\n")
cat("  Calculated General_Effect_Total and Music_Eff_Avg_Total for all records\n\n")

# ============================================================================
# STEP 4: Validate the new data
# ============================================================================

cat("STEP 4: Validating new data\n")
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
        # Check Timing_Penalty_Tot (-10 to 0)
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
    print(invalid_summary, n = 20)

    # Filter to valid records only
    new_scores_df_valid <- new_scores_df %>%
        filter(
            # All 0-20 range fields must be valid
            if_all(all_of(all_validated_fields), ~ is.na(.) | (. >= 0 & . <= 20)) &
            # Timing_Penalty_Tot must be between -10 and 0
            (is.na(Timing_Penalty_Tot) | (Timing_Penalty_Tot >= -10 & Timing_Penalty_Tot <= 0))
        )

    cat("\n✓ Proceeding with", nrow(new_scores_df_valid), "valid records\n\n")
} else {
    new_scores_df_valid <- new_scores_df
    cat("\n✓ All new records are valid\n\n")
}

# ============================================================================
# STEP 5: Combine with existing data
# ============================================================================

cat("STEP 5: Combining with existing data\n")
cat("=====================================\n")

# Load existing data (scores_labeled_df is already loaded from read_file.R)
cat("Existing records:", nrow(scores_labeled_df), "\n")
cat("New valid records:", nrow(new_scores_df_valid), "\n")

# Convert Event_Date to Date type in new data (if it's character)
if (is.character(new_scores_df_valid$Event_Date)) {
    cat("\nConverting Event_Date from character to Date...\n")
    new_scores_df_valid <- new_scores_df_valid %>%
        mutate(Event_Date = as.Date(Event_Date, format = "%B %d, %Y"))
}

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
cat("  Date range:", format(min(combined_scores_df$Event_Date, na.rm = TRUE)), "to",
    format(max(combined_scores_df$Event_Date, na.rm = TRUE)), "\n")

# ============================================================================
# STEP 6: Save results
# ============================================================================

cat("\nSTEP 6: Saving results\n")
cat("======================\n")

# Save combined dataset
output_file <- "data/combined_scores.csv"
write.csv(combined_scores_df, output_file, row.names = FALSE)
cat("✓ Combined data saved to:", output_file, "\n")

# Save PDF link list
links_file <- "data/downloaded_pdf_links.txt"
writeLines(all_pdf_links, links_file)
cat("✓ PDF links saved to:", links_file, "\n")

# Save validation report
validation_file <- "data/download_validation_report.txt"
sink(validation_file)
cat("=== Download and Validation Report ===\n")
cat("Generated:", format(Sys.time()), "\n\n")

cat("Base URLs Scraped:\n")
for (i in seq_along(base_urls)) {
    cat("  ", i, ".", base_urls[i], "\n")
}

cat("\nDownload Summary:\n")
cat("  PDF links found:", length(all_pdf_links), "\n")
cat("  Files downloaded:", length(downloaded_files) - skipped_files, "\n")
cat("  Files already existed:", skipped_files, "\n")
cat("  Failed downloads:", failed_downloads, "\n")

cat("\nExtraction Summary:\n")
cat("  Files processed:", length(downloaded_files), "\n")
cat("  Files with data:", length(new_scores_list), "\n")
cat("  Extraction errors:", extraction_errors, "\n")
cat("  Records extracted:", nrow(new_scores_df), "\n")

cat("\nValidation Summary:\n")
cat("  Valid records:", nrow(new_scores_df_valid), "\n")
cat("  Invalid records:", nrow(new_invalid), "\n")

if (nrow(new_invalid) > 0) {
    cat("\nInvalid Schools:\n")
    print(invalid_summary)
}

cat("\nCombined Dataset:\n")
cat("  Total records:", nrow(combined_scores_df), "\n")
cat("  Total schools:", n_distinct(combined_scores_df$School), "\n")
cat("  Total competitions:", n_distinct(combined_scores_df$Competition_Name), "\n")
cat("  Date range:", format(min(combined_scores_df$Event_Date, na.rm = TRUE)), "to",
    format(max(combined_scores_df$Event_Date, na.rm = TRUE)), "\n")

if (nrow(duplicate_check) > 0) {
    cat("\nDuplicate Records Found:\n")
    print(dup_summary)
}

sink()
cat("✓ Validation report saved to:", validation_file, "\n")

cat("\n=== WORKFLOW COMPLETE ===\n")
cat("\nDatasets available:\n")
cat("  - combined_scores_df: All data (", nrow(combined_scores_df), " records)\n", sep = "")
cat("  - new_scores_df_valid: Only new data (", nrow(new_scores_df_valid), " records)\n", sep = "")
cat("  - scores_labeled_df: Original data (", nrow(scores_labeled_df), " records)\n", sep = "")
cat("\nNext steps:\n")
cat("  1. Review validation report:", validation_file, "\n")
cat("  2. Run analysis: source('R/correlation_matrix.R')\n")
cat("  3. Create visualizations: source('R/radar_plot_ggplot.R')\n")
