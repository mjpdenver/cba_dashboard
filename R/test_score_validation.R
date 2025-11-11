library(dplyr)

source('R/read_file.R')

cat("=== Score Validation Report ===\n\n")

# Define expected ranges for different types of columns
# Rule: Fields ending in "_Tot" should be 0-20
# Music_Total, Visual_Total should also be 0-20
# Sub_Total, Weighted_Total, Final_Total can exceed 20 (they are aggregates)
component_scores_tot <- c("Music_Ind_Tot", "Music_Ens_Tot",
                          "Visual_Ind_Tot", "Visual_Ens_Tot",
                          "Music_Eff1_Tot", "Music_Eff2_Tot", "Visual_Eff_Tot")

component_scores_total <- c("Music_Total", "Visual_Total")

aggregate_scores <- c("Sub_Total", "Weighted_Total", "Final_Total")
penalty_scores <- c("Timing_Penalty", "Timing_Penalty_Tot")

cat("Fields Ending in '_Tot' (Expected Range: 0-20):\n")
cat("================================================\n\n")

issues_found <- FALSE

for (col in component_scores_tot) {
    if (col %in% names(scores_labeled_df)) {
        values <- scores_labeled_df[[col]]
        out_of_range <- scores_labeled_df %>%
            filter(!is.na(.data[[col]]) & (.data[[col]] < 0 | .data[[col]] > 20))

        if (nrow(out_of_range) > 0) {
            issues_found <- TRUE
            cat("✗", col, "- ISSUE FOUND\n")
            cat("  Out of range:", nrow(out_of_range), "schools\n")
            cat("  Range:", round(min(values, na.rm = TRUE), 2), "-",
                round(max(values, na.rm = TRUE), 2), "\n")
            cat("  Schools with issues:\n")
            problem_schools <- out_of_range %>%
                select(School, Competition_Name, Event_Date, all_of(col)) %>%
                arrange(desc(.data[[col]])) %>%
                head(10)
            print(problem_schools)
            cat("\n")
        } else {
            cat("✓", col, "- OK (Range:",
                round(min(values, na.rm = TRUE), 2), "-",
                round(max(values, na.rm = TRUE), 2), ")\n")
        }
    }
}

cat("\n\nComponent '_Total' Fields (Expected Range: 0-20):\n")
cat("==================================================\n\n")

for (col in component_scores_total) {
    if (col %in% names(scores_labeled_df)) {
        values <- scores_labeled_df[[col]]
        out_of_range <- scores_labeled_df %>%
            filter(!is.na(.data[[col]]) & (.data[[col]] < 0 | .data[[col]] > 20))

        if (nrow(out_of_range) > 0) {
            issues_found <- TRUE
            cat("✗", col, "- ISSUE FOUND\n")
            cat("  Out of range:", nrow(out_of_range), "schools\n")
            cat("  Range:", round(min(values, na.rm = TRUE), 2), "-",
                round(max(values, na.rm = TRUE), 2), "\n")
            cat("  Schools with issues:\n")
            problem_schools <- out_of_range %>%
                select(School, Competition_Name, Event_Date, all_of(col)) %>%
                arrange(desc(.data[[col]])) %>%
                head(10)
            print(problem_schools)
            cat("\n")
        } else {
            cat("✓", col, "- OK (Range:",
                round(min(values, na.rm = TRUE), 2), "-",
                round(max(values, na.rm = TRUE), 2), ")\n")
        }
    }
}

cat("\n\nAggregate '_Total' Fields (Can exceed 20):\n")
cat("===========================================\n")
for (col in aggregate_scores) {
    if (col %in% names(scores_labeled_df)) {
        values <- scores_labeled_df[[col]]
        cat("•", col, "- Range:",
            round(min(values, na.rm = TRUE), 2), "-",
            round(max(values, na.rm = TRUE), 2), "\n")
    }
}

cat("\n\nPenalty Scores:\n")
cat("===============\n")
for (col in penalty_scores) {
    if (col %in% names(scores_labeled_df)) {
        values <- scores_labeled_df[[col]]

        if (col == "Timing_Penalty_Tot") {
            # Timing_Penalty_Tot should be between -10 and 0
            out_of_range <- scores_labeled_df %>%
                filter(!is.na(.data[[col]]) & (.data[[col]] < -10 | .data[[col]] > 0))

            if (nrow(out_of_range) > 0) {
                issues_found <- TRUE
                cat("✗", col, "- ISSUE FOUND (expected: -10 to 0)\n")
                cat("  Out of range:", nrow(out_of_range), "schools\n")
                cat("  Range:", round(min(values, na.rm = TRUE), 2), "-",
                    round(max(values, na.rm = TRUE), 2), "\n")
                cat("  Schools with issues:\n")
                problem_schools <- out_of_range %>%
                    select(School, Competition_Name, Event_Date, all_of(col)) %>%
                    arrange(desc(.data[[col]])) %>%
                    head(10)
                print(problem_schools)
                cat("\n")
            } else {
                cat("✓", col, "- OK (Range:",
                    round(min(values, na.rm = TRUE), 2), "-",
                    round(max(values, na.rm = TRUE), 2), ")\n")
            }
        } else {
            # Timing_Penalty can be any value
            cat("•", col, "- Range:",
                round(min(values, na.rm = TRUE), 2), "-",
                round(max(values, na.rm = TRUE), 2), "\n")
        }
    }
}

cat("\n\n=== SUMMARY ===\n")
if (issues_found) {
    cat("✗ VALIDATION FAILED - Score fields found outside expected ranges\n")
    cat("\nValidation Rules:\n")
    cat("  - Fields ending in '_Tot': must be 0-20\n")
    cat("  - Music_Total, Visual_Total: must be 0-20\n")
    cat("  - Sub_Total, Weighted_Total, Final_Total: can exceed 20 (aggregates)\n")
    cat("  - Timing_Penalty_Tot: must be -10 to 0\n")
    cat("\nRecommendation: Review data extraction for schools with out-of-range values.\n")
    cat("These may indicate:\n")
    cat("  - PDF formatting differences\n")
    cat("  - Raw judge scores not properly filtered\n")
    cat("  - Missing data being filled with incorrect values\n")
} else {
    cat("✓ VALIDATION PASSED - All score fields within expected ranges\n")
}

cat("\nTotal schools validated:", nrow(scores_labeled_df), "\n")
