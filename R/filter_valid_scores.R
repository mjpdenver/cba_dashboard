# Filter scores_labeled_df to only include valid records
library(dplyr)

source('R/read_file.R')

cat("=== Filtering Valid Score Records ===\n\n")

# Define validation rules
# Fields that should be 0-20
component_scores_tot <- c("Music_Ind_Tot", "Music_Ens_Tot",
                          "Visual_Ind_Tot", "Visual_Ens_Tot",
                          "Music_Eff1_Tot", "Music_Eff2_Tot", "Visual_Eff_Tot")

component_scores_total <- c("Music_Total", "Visual_Total")

# Timing_Penalty_Tot should be between -10 and 0
timing_penalty_field <- "Timing_Penalty_Tot"

all_validated_fields <- c(component_scores_tot, component_scores_total)

# Starting count
cat("Starting records:", nrow(scores_labeled_df), "\n\n")

# Identify invalid records
invalid_records <- scores_labeled_df %>%
    filter(
        # Check 0-20 range fields
        if_any(all_of(all_validated_fields), ~ !is.na(.) & (. < 0 | . > 20)) |
        # Check Timing_Penalty_Tot (-10 to 0)
        (!is.na(Timing_Penalty_Tot) & (Timing_Penalty_Tot < -10 | Timing_Penalty_Tot > 0))
    )

cat("Invalid records found:", nrow(invalid_records), "\n")

if (nrow(invalid_records) > 0) {
    cat("\nSchools with invalid scores:\n")
    invalid_summary <- invalid_records %>%
        select(School, Competition_Name, Event_Date) %>%
        distinct() %>%
        arrange(School, Event_Date)
    print(invalid_summary, n = Inf)

    cat("\n\nFields with out-of-range values in invalid records:\n")
    for (col in all_validated_fields) {
        out_of_range <- invalid_records %>%
            filter(!is.na(.data[[col]]) & (.data[[col]] < 0 | .data[[col]] > 20))

        if (nrow(out_of_range) > 0) {
            cat("  •", col, ":", nrow(out_of_range), "records (expected: 0-20)\n")
        }
    }

    # Check Timing_Penalty_Tot separately
    if (timing_penalty_field %in% names(invalid_records)) {
        penalty_out_of_range <- invalid_records %>%
            filter(!is.na(Timing_Penalty_Tot) & (Timing_Penalty_Tot < -10 | Timing_Penalty_Tot > 0))

        if (nrow(penalty_out_of_range) > 0) {
            cat("  •", timing_penalty_field, ":", nrow(penalty_out_of_range), "records (expected: -10 to 0)\n")
        }
    }
}

# Create filtered dataset with only valid records
scores_labeled_df_valid <- scores_labeled_df %>%
    filter(
        # All 0-20 range fields must be valid
        if_all(all_of(all_validated_fields), ~ is.na(.) | (. >= 0 & . <= 20)) &
        # Timing_Penalty_Tot must be between -10 and 0
        (is.na(Timing_Penalty_Tot) | (Timing_Penalty_Tot >= -10 & Timing_Penalty_Tot <= 0))
    )

cat("\n\n=== FILTERING COMPLETE ===\n")
cat("Valid records:", nrow(scores_labeled_df_valid), "\n")
cat("Invalid records removed:", nrow(scores_labeled_df) - nrow(scores_labeled_df_valid), "\n")
cat("Percentage valid:", round(100 * nrow(scores_labeled_df_valid) / nrow(scores_labeled_df), 1), "%\n")

cat("\n✓ Filtered dataset available as: scores_labeled_df_valid\n")
cat("  Original dataset still available as: scores_labeled_df\n")
