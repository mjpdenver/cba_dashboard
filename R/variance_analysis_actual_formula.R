library(dplyr)
library(ggplot2)
library(tidyr)

cat("=== Variance Contribution Analysis (Using Actual Formula) ===\n\n")

# Load validated band competition data
source('R/read_file.R')

# ============================================================================
# THE ACTUAL FORMULA
# ============================================================================

cat("ACTUAL FORMULA:\n")
cat("Final_Total = Music_Ind_Tot + Music_Ens_Tot + Visual_Ind_Tot + Visual_Ens_Tot +\n")
cat("              3 * (Music_Eff_Avg_Total + Visual_Eff_Tot)\n\n")

# Coefficients from the actual formula
actual_coefficients <- data.frame(
    Predictor = c("Music_Ind_Tot", "Music_Ens_Tot", "Visual_Ind_Tot",
                  "Visual_Ens_Tot", "Music_Eff_Avg_Total", "Visual_Eff_Tot"),
    Coefficient = c(1, 1, 1, 1, 3, 3)
)

cat("Coefficients:\n")
print(actual_coefficients)
cat("\n")

# ============================================================================
# STEP 1: Verify the formula
# ============================================================================

cat("Step 1: Verifying the formula\n\n")

model_data <- scores_labeled_df_valid %>%
    select(Final_Total, Music_Ind_Tot, Music_Ens_Tot, Visual_Ind_Tot,
           Visual_Ens_Tot, Music_Eff_Avg_Total, Visual_Eff_Tot) %>%
    drop_na()

verification <- model_data %>%
    mutate(
        Calculated = Music_Ind_Tot + Music_Ens_Tot + Visual_Ind_Tot +
                     Visual_Ens_Tot + 3 * (Music_Eff_Avg_Total + Visual_Eff_Tot),
        Difference = abs(Final_Total - Calculated)
    )

cat("Sample verification (first 10 rows):\n")
print(head(verification %>% select(Final_Total, Calculated, Difference), 10))
cat("\n")
cat("Max difference:", max(verification$Difference), "\n")
cat("Mean difference:", mean(verification$Difference), "\n")
cat("Perfect matches (diff < 0.01):", sum(verification$Difference < 0.01),
    "out of", nrow(verification), "\n\n")

# ============================================================================
# STEP 2: Calculate variance of each predictor
# ============================================================================

cat("Step 2: Variance of each predictor\n\n")

predictors <- c("Music_Ind_Tot", "Music_Ens_Tot", "Visual_Ind_Tot",
                "Visual_Ens_Tot", "Music_Eff_Avg_Total", "Visual_Eff_Tot")

variance_stats <- model_data %>%
    summarise(across(all_of(predictors), list(
        mean = ~mean(.),
        sd = ~sd(.),
        var = ~var(.)
    ))) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    separate(metric, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = stat, values_from = value)

print(variance_stats)
cat("\n")

# ============================================================================
# STEP 3: Calculate variance contribution using actual coefficients
# ============================================================================

cat("Step 3: Variance contribution using actual formula coefficients\n\n")

contribution_table <- variance_stats %>%
    left_join(actual_coefficients, by = c("variable" = "Predictor")) %>%
    mutate(
        Variance_Contribution = var * Coefficient^2,
        Percent_Contribution = 100 * Variance_Contribution / sum(Variance_Contribution)
    ) %>%
    arrange(desc(Variance_Contribution)) %>%
    select(variable, mean, sd, var, Coefficient, Variance_Contribution, Percent_Contribution)

cat("Variance Contribution (sorted by contribution):\n")
print(contribution_table, n = Inf)
cat("\n")

# ============================================================================
# STEP 4: Total variance explained
# ============================================================================

cat("Step 4: Total variance in Final_Total\n\n")

final_total_var <- var(model_data$Final_Total)
sum_contributions <- sum(contribution_table$Variance_Contribution)

cat("Variance of Final_Total:", round(final_total_var, 2), "\n")
cat("Sum of variance contributions:", round(sum_contributions, 2), "\n")
cat("Match:", if(abs(final_total_var - sum_contributions) < 0.1) "✓ Perfect" else "✗ Mismatch", "\n\n")

# ============================================================================
# STEP 5: Detailed breakdown
# ============================================================================

cat("=== SUMMARY ===\n\n")

cat("VARIANCE CONTRIBUTION TO FINAL_TOTAL:\n\n")

for (i in 1:nrow(contribution_table)) {
    row <- contribution_table[i, ]
    cat(sprintf("%d. %-20s: %5.1f%% (Coef: %.0f, Var: %.2f)\n",
                i, row$variable, row$Percent_Contribution,
                row$Coefficient, row$var))
}

cat("\n")

# Group by coefficient level
cat("GROUPED BY COEFFICIENT:\n")
cat("  Effect scores (3× weight):\n")
effect_pct <- contribution_table %>%
    filter(Coefficient == 3) %>%
    pull(Percent_Contribution) %>%
    sum()
cat("    Music_Eff_Avg_Total + Visual_Eff_Tot =", round(effect_pct, 1), "%\n\n")

cat("  Performance scores (1× weight):\n")
perf_pct <- contribution_table %>%
    filter(Coefficient == 1) %>%
    pull(Percent_Contribution) %>%
    sum()
cat("    Music_Ind + Music_Ens + Visual_Ind + Visual_Ens =", round(perf_pct, 1), "%\n\n")

# ============================================================================
# STEP 6: Sensitivity analysis
# ============================================================================

cat("Step 6: Sensitivity Analysis\n\n")
cat("If a school improves by 1 point in each category, Final_Total increases by:\n\n")

for (i in 1:nrow(contribution_table)) {
    row <- contribution_table[i, ]
    cat(sprintf("  %-20s: +%.0f point(s)\n", row$variable, row$Coefficient))
}

cat("\n")

# ============================================================================
# STEP 7: Visualizations
# ============================================================================

cat("=== Creating Visualizations ===\n\n")

# 1. Variance contribution with actual coefficients
p1 <- ggplot(contribution_table,
             aes(x = reorder(variable, Percent_Contribution),
                 y = Percent_Contribution, fill = as.factor(Coefficient))) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = paste0(round(Percent_Contribution, 1), "%")),
              hjust = -0.1, size = 4) +
    scale_fill_manual(values = c("1" = "steelblue", "3" = "darkred"),
                      name = "Coefficient",
                      labels = c("1" = "Performance (1×)", "3" = "Effect (3×)")) +
    labs(
        title = "Variance Contribution to Final_Total (Actual Formula)",
        subtitle = "Final_Total = Performance scores (1×) + Effect scores (3×)",
        x = "Predictor",
        y = "% of Total Variance Explained"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ylim(0, max(contribution_table$Percent_Contribution) * 1.15)

ggsave("variance_contribution_actual.png", p1, width = 10, height = 6)
cat("✓ Saved: variance_contribution_actual.png\n")

# 2. Pie chart of effect vs performance
pie_data <- data.frame(
    Category = c("Effect Scores (3×)", "Performance Scores (1×)"),
    Percentage = c(effect_pct, perf_pct)
)

p2 <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")),
              position = position_stack(vjust = 0.5), size = 6) +
    scale_fill_manual(values = c("Effect Scores (3×)" = "darkred",
                                  "Performance Scores (1×)" = "steelblue")) +
    labs(title = "Variance Contribution: Effect vs Performance") +
    theme_void() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 14))

ggsave("effect_vs_performance_pie.png", p2, width = 8, height = 8)
cat("✓ Saved: effect_vs_performance_pie.png\n")

# 3. Coefficient × Variance visualization
p3 <- ggplot(contribution_table,
             aes(x = var, y = Variance_Contribution,
                 color = as.factor(Coefficient), size = Coefficient)) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = variable), hjust = -0.1, vjust = 0, size = 3) +
    scale_color_manual(values = c("1" = "steelblue", "3" = "darkred"),
                       name = "Coefficient") +
    scale_size_continuous(range = c(4, 10)) +
    labs(
        title = "Variance × Coefficient² = Contribution",
        subtitle = "Effect scores (3×) dominate due to higher coefficient",
        x = "Variance of Predictor",
        y = "Variance Contribution to Final_Total"
    ) +
    theme_minimal() +
    guides(size = "none")

ggsave("variance_times_coefficient.png", p3, width = 10, height = 6)
cat("✓ Saved: variance_times_coefficient.png\n\n")

# ============================================================================
# STEP 8: Save results
# ============================================================================

cat("=== Saving Results ===\n\n")

write.csv(contribution_table, "data/variance_contribution_actual.csv", row.names = FALSE)
cat("✓ Saved: data/variance_contribution_actual.csv\n")

# Summary table
summary_table <- data.frame(
    Category = c("Effect Scores (Music_Eff_Avg_Total)",
                 "Effect Scores (Visual_Eff_Tot)",
                 "Effect Scores TOTAL",
                 "Performance Scores TOTAL"),
    Coefficient = c(3, 3, NA, 1),
    Variance_Contribution_Pct = c(
        contribution_table$Percent_Contribution[contribution_table$variable == "Music_Eff_Avg_Total"],
        contribution_table$Percent_Contribution[contribution_table$variable == "Visual_Eff_Tot"],
        effect_pct,
        perf_pct
    )
)

write.csv(summary_table, "data/variance_summary.csv", row.names = FALSE)
cat("✓ Saved: data/variance_summary.csv\n\n")

cat("✓ Analysis complete!\n\n")

cat("KEY FINDING:\n")
cat("Effect scores (with 3× coefficient) explain", round(effect_pct, 1),
    "% of variance in Final_Total\n")
cat("Performance scores (with 1× coefficient) explain", round(perf_pct, 1),
    "% of variance\n")
