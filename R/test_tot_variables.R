library(dplyr)
library(tidyr)

source('R/read_file.R')

cat("=== Testing Score Variables (0-20 Range) ===\n\n")
cat("Validation Rules:\n")
cat("  - Fields ending in '_Tot': must be 0-20\n")
cat("  - Music_Total, Visual_Total: must be 0-20\n")
cat("  - Sub_Total, Weighted_Total, Final_Total: can exceed 20 (not tested)\n\n")

# Find all columns ending in "_Tot" but NOT "_Total" (case insensitive)
tot_cols <- names(scores_labeled_df)[grepl("_Tot$", names(scores_labeled_df), ignore.case = TRUE) &
                                       !grepl("_Total$", names(scores_labeled_df), ignore.case = TRUE)]

# Add component _Total fields that should also be 0-20
component_total_cols <- c("Music_Total", "Visual_Total")
component_total_cols <- component_total_cols[component_total_cols %in% names(scores_labeled_df)]

# Combine all columns to test
all_test_cols <- c(tot_cols, component_total_cols)

cat("Found", length(tot_cols), "columns ending in '_Tot':\n")
print(tot_cols)
cat("\nAdditional component '_Total' fields to test (0-20 range):\n")
print(component_total_cols)
cat("\nTotal columns to test:", length(all_test_cols), "\n\n")

# Test each column
results <- data.frame(
    Column = character(),
    Min = numeric(),
    Max = numeric(),
    NA_Count = integer(),
    Below_0 = integer(),
    Above_20 = integer(),
    In_Range = integer(),
    Pass = logical(),
    stringsAsFactors = FALSE
)

for (col in all_test_cols) {
    values <- scores_labeled_df[[col]]

    # Calculate statistics
    min_val <- min(values, na.rm = TRUE)
    max_val <- max(values, na.rm = TRUE)
    na_count <- sum(is.na(values))
    below_0 <- sum(values < 0, na.rm = TRUE)
    above_20 <- sum(values > 20, na.rm = TRUE)
    in_range <- sum(values >= 0 & values <= 20, na.rm = TRUE)

    # Determine if column passes (all non-NA values between 0 and 20)
    pass <- (below_0 == 0 && above_20 == 0)

    results <- rbind(results, data.frame(
        Column = col,
        Min = round(min_val, 2),
        Max = round(max_val, 2),
        NA_Count = na_count,
        Below_0 = below_0,
        Above_20 = above_20,
        In_Range = in_range,
        Pass = pass
    ))
}

# Display results
cat("Test Results:\n")
cat("=============\n\n")

# Show passing columns
passing <- results[results$Pass, ]
if (nrow(passing) > 0) {
    cat("✓ PASSING (", nrow(passing), " columns in 0-20 range):\n", sep = "")
    print(passing[, c("Column", "Min", "Max", "In_Range", "NA_Count")], row.names = FALSE)
    cat("\n")
}

# Show failing columns
failing <- results[!results$Pass, ]
if (nrow(failing) > 0) {
    cat("✗ FAILING (", nrow(failing), " columns outside 0-20 range):\n", sep = "")
    print(failing, row.names = FALSE)
    cat("\n")

    # Show examples of out-of-range values
    for (col in failing$Column) {
        cat("\nExamples for", col, ":\n")
        values <- scores_labeled_df[[col]]

        if (any(values < 0, na.rm = TRUE)) {
            below <- scores_labeled_df[!is.na(values) & values < 0, c("School", "Competition_Name", col)]
            cat("  Below 0 (", nrow(below), " schools):\n", sep = "")
            print(head(below, 5))
        }

        if (any(values > 20, na.rm = TRUE)) {
            above <- scores_labeled_df[!is.na(values) & values > 20, c("School", "Competition_Name", col)]
            cat("  Above 20 (", nrow(above), " schools):\n", sep = "")
            print(head(above, 5))
        }
    }
}

# Summary
cat("\n\n=== SUMMARY ===\n")
cat("Total columns tested:", nrow(results), "\n")
cat("Passing:", sum(results$Pass), "\n")
cat("Failing:", sum(!results$Pass), "\n")

if (all(results$Pass)) {
    cat("\n✓ ALL TESTS PASSED! All score fields are within expected 0-20 range.\n")
} else {
    cat("\n✗ TESTS FAILED. Some score fields are outside 0-20 range.\n")
    cat("\nValidation Rules:\n")
    cat("  - Fields ending in '_Tot': must be 0-20\n")
    cat("  - Music_Total, Visual_Total: must be 0-20\n")
    cat("  - Sub_Total, Weighted_Total, Final_Total: can exceed 20 (not tested)\n")
}
