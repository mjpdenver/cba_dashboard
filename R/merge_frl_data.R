library(dplyr)
library(stringr)

# Load validated band competition scores
source('R/read_file.R')

cat("=== Merge Free/Reduced Lunch Data with Band Scores ===\n\n")

# ============================================================================
# STEP 1: Load FRL data (user should update this path)
# ============================================================================

# Path to FRL data file (update this after downloading from CDE)
frl_file <- "data/frl_data.csv"  # or .xlsx

if (!file.exists(frl_file)) {
    cat("✗ FRL data file not found:", frl_file, "\n")
    cat("\nPlease download FRL data from:\n")
    cat("  https://www.cde.state.co.us/cdereval/pupilcurrent\n")
    cat("  or https://www.cde.state.co.us/cdereval/2022-2023schoolk-12frl\n\n")
    cat("Save the file as:", frl_file, "\n")
    cat("Expected columns: School Name, District Name, FRL Percentage (or similar)\n")
    stop("FRL data file not found")
}

cat("Step 1: Loading FRL data\n")
cat("  File:", frl_file, "\n")

# Load FRL data (adjust column names as needed based on actual file structure)
# Common CDE column names: "SCHOOL_NAME", "FRL_PERCENT", "FREE_REDUCED_ELIGIBLE"
frl_data <- read.csv(frl_file, stringsAsFactors = FALSE)

cat("  Rows:", nrow(frl_data), "\n")
cat("  Columns:", paste(names(frl_data), collapse = ", "), "\n\n")

# ============================================================================
# STEP 2: Standardize school names for matching
# ============================================================================

cat("Step 2: Standardizing school names\n")

# Function to standardize school names
standardize_name <- function(name) {
    name %>%
        str_to_upper() %>%                                         # Convert to uppercase
        str_replace_all("\\s+", " ") %>%                           # Normalize whitespace
        # Fix compound names that sometimes appear without spaces
        str_replace_all("\\bLAJUNTA\\b", "LA JUNTA") %>%          # LaJunta → La Junta
        # IMPORTANT: Longer patterns must come FIRST!
        str_replace_all("MIDDLE/HIGH SCHOOL$", "") %>%
        str_replace_all("INTERNATIONAL JUNIOR/SENIOR HIGH SCHOOL$", "") %>%
        str_replace_all("JUNIOR/SENIOR HIGH SCHOOL$", "") %>%
        str_replace_all("JR\\./SR\\. HIGH SCHOOL$", "") %>%
        str_replace_all("JR/SR HIGH SCHOOL$", "") %>%
        str_replace_all("SENIOR HIGH SCHOOL$", "") %>%
        str_replace_all("HIGH SCHOOL$", "") %>%                    # Shorter pattern last
        str_replace_all("\\bHIGH$", "") %>%                        # Remove "HIGH" at end
        str_replace_all("\\bHS$", "") %>%                          # Remove "HS" at end
        str_replace_all("\\s+", " ") %>%                           # Normalize whitespace again
        str_trim()                                                 # Remove leading/trailing spaces
}

# Get unique schools from band data
band_schools <- scores_labeled_df_valid %>%
    select(School) %>%
    distinct() %>%
    mutate(School_Standard = standardize_name(School)) %>%
    arrange(School)

cat("  Band competition schools:", nrow(band_schools), "\n")

# Standardize FRL school names (adjust column name as needed)
# Assuming the FRL file has a column like "SCHOOL_NAME" - update if different
frl_school_col <- names(frl_data)[str_detect(names(frl_data), "(?i)school.*name")][1]
frl_percent_col <- names(frl_data)[str_detect(names(frl_data), "(?i)X\\.\\.Free\\.and\\.Reduced|frl|free.*reduce|percent")][1]
enrollment_col <- names(frl_data)[str_detect(names(frl_data), "(?i)total.*pupil|enrollment|pk.*12.*count|total.*count")][1]

if (is.na(frl_school_col) || is.na(frl_percent_col)) {
    cat("\n✗ Could not auto-detect FRL column names\n")
    cat("Available columns:", paste(names(frl_data), collapse = ", "), "\n")
    cat("\nPlease update the script with correct column names\n")
    stop("Column detection failed")
}

cat("  Detected columns:\n")
cat("    School name:", frl_school_col, "\n")
cat("    FRL percent:", frl_percent_col, "\n")

# Check if enrollment data is available
has_enrollment <- !is.na(enrollment_col)
if (has_enrollment) {
    cat("    Enrollment:", enrollment_col, "✓\n\n")
} else {
    cat("    Enrollment: Not found (will be set to NA)\n\n")
}

# Select columns (include enrollment if available)
if (has_enrollment) {
    frl_data_clean <- frl_data %>%
        select(School_Name_Original = all_of(frl_school_col),
               FRL_Percent = all_of(frl_percent_col),
               School_Size = all_of(enrollment_col)) %>%
        mutate(
            # Convert to numeric (handles strings like "45.2%" or "1,234")
            FRL_Percent = as.numeric(gsub("[^0-9.]", "", as.character(FRL_Percent))),
            School_Size = as.numeric(gsub("[^0-9]", "", as.character(School_Size))),
            School_Standard = standardize_name(School_Name_Original)
        ) %>%
        filter(!is.na(School_Name_Original))
} else {
    frl_data_clean <- frl_data %>%
        select(School_Name_Original = all_of(frl_school_col),
               FRL_Percent = all_of(frl_percent_col)) %>%
        mutate(
            # Convert to numeric (handles strings like "45.2%")
            FRL_Percent = as.numeric(gsub("[^0-9.]", "", as.character(FRL_Percent))),
            School_Size = NA_real_,
            School_Standard = standardize_name(School_Name_Original)
        ) %>%
        filter(!is.na(School_Name_Original))
}

# ============================================================================
# STEP 3: Match schools
# ============================================================================

cat("Step 3: Matching schools\n")

# Join band schools with FRL data
matched_schools <- band_schools %>%
    left_join(frl_data_clean, by = "School_Standard") %>%
    mutate(Match_Status = if_else(is.na(FRL_Percent), "No Match", "Matched"))

# Summary
matched_count <- sum(matched_schools$Match_Status == "Matched")
unmatched_count <- sum(matched_schools$Match_Status == "No Match")

cat("  Matched:", matched_count, "\n")
cat("  Unmatched:", unmatched_count, "\n\n")

# Show unmatched schools
if (unmatched_count > 0) {
    cat("⚠ Unmatched schools:\n")
    unmatched <- matched_schools %>%
        filter(Match_Status == "No Match") %>%
        select(School, School_Standard)
    print(unmatched, n = Inf)
    cat("\n")

    # Save unmatched schools for manual review
    write.csv(unmatched, "data/frl_unmatched_schools.csv", row.names = FALSE)
    cat("✓ Saved unmatched schools to: data/frl_unmatched_schools.csv\n\n")
}

# ============================================================================
# STEP 4: Merge with full band data
# ============================================================================

cat("Step 4: Merging FRL data with band scores\n")

# Create lookup table
frl_lookup <- matched_schools %>%
    filter(Match_Status == "Matched") %>%
    select(School, FRL_Percent, School_Size)

# Merge with validated band scores
scores_with_frl <- scores_labeled_df_valid %>%
    left_join(frl_lookup, by = "School")

cat("  Total records:", nrow(scores_with_frl), "\n")
cat("  Records with FRL data:", sum(!is.na(scores_with_frl$FRL_Percent)), "\n")
cat("  Records without FRL data:", sum(is.na(scores_with_frl$FRL_Percent)), "\n")
if (has_enrollment) {
    cat("  Records with enrollment data:", sum(!is.na(scores_with_frl$School_Size)), "\n")
}
cat("\n")

# ============================================================================
# STEP 5: Save results
# ============================================================================

cat("Step 5: Saving results\n")

# Save merged data
output_file <- "data/band_scores_with_frl.csv"
write.csv(scores_with_frl, output_file, row.names = FALSE)
cat("✓ Saved merged data to:", output_file, "\n")

# Save matching summary
summary_file <- "data/frl_matching_summary.csv"
write.csv(matched_schools, summary_file, row.names = FALSE)
cat("✓ Saved matching summary to:", summary_file, "\n\n")

# ============================================================================
# STEP 6: Data summary
# ============================================================================

cat("=== Summary Statistics ===\n\n")

# FRL statistics by school
frl_summary <- scores_with_frl %>%
    filter(!is.na(FRL_Percent)) %>%
    group_by(School, FRL_Percent, School_Size) %>%
    summarize(
        Competitions = n(),
        Avg_Final_Score = mean(Final_Total, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(FRL_Percent))

cat("Schools with FRL data (showing top 10 by FRL%):\n")
print(head(frl_summary, 10))

cat("\n\nFRL Distribution:\n")
cat("  Min:", min(scores_with_frl$FRL_Percent, na.rm = TRUE), "%\n")
cat("  Mean:", round(mean(scores_with_frl$FRL_Percent, na.rm = TRUE), 1), "%\n")
cat("  Median:", median(scores_with_frl$FRL_Percent, na.rm = TRUE), "%\n")
cat("  Max:", max(scores_with_frl$FRL_Percent, na.rm = TRUE), "%\n")

if (has_enrollment && sum(!is.na(scores_with_frl$School_Size)) > 0) {
    cat("\nSchool Size Distribution:\n")
    cat("  Min:", min(scores_with_frl$School_Size, na.rm = TRUE), "students\n")
    cat("  Mean:", round(mean(scores_with_frl$School_Size, na.rm = TRUE), 0), "students\n")
    cat("  Median:", median(scores_with_frl$School_Size, na.rm = TRUE), "students\n")
    cat("  Max:", max(scores_with_frl$School_Size, na.rm = TRUE), "students\n")
}
cat("\n")

cat("✓ Merge complete!\n\n")
cat("Next steps:\n")
cat("  1. Review unmatched schools (if any) in data/frl_unmatched_schools.csv\n")
cat("  2. Use 'scores_with_frl' dataframe for analysis\n")
cat("  3. Run correlation analysis: source('R/frl_correlation.R')\n")

scores_with_frl$FRL_Percent <-  
    scores_with_frl$FRL_Percent/scores_with_frl$School_Size
