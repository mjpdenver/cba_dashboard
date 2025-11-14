library(dplyr)
library(ggplot2)

cat("=== FRL and Band Performance Correlation Analysis ===\n\n")

# Load merged data
if (!file.exists("data/band_scores_with_frl.csv")) {
    cat("✗ Merged data not found. Please run 'source(\"R/merge_frl_data.R\")' first.\n")
    stop("Data not found")
}

scores_with_frl <- read.csv("data/band_scores_with_frl.csv", stringsAsFactors = FALSE)

# Filter to records with FRL data
analysis_data <- scores_with_frl %>%
    filter(!is.na(FRL_Percent) & !is.na(Final_Total))

cat("Records with both FRL and score data:", nrow(analysis_data), "\n")
cat("Unique schools:", n_distinct(analysis_data$School), "\n\n")

# ============================================================================
# Correlation Analysis
# ============================================================================

cat("=== Correlation Results ===\n\n")

# Correlations with different score metrics
score_vars <- c("Final_Total", "Music_Total", "Visual_Total",
                "Music_Eff_Avg_Total", "General_Effect_Total")

for (var in score_vars) {
    if (var %in% names(analysis_data)) {
        data_clean <- analysis_data %>%
            filter(!is.na(.data[[var]]))

        if (nrow(data_clean) > 0) {
            cor_val <- cor(data_clean$FRL_Percent, data_clean[[var]],
                          use = "complete.obs")

            # Linear model for significance
            model <- lm(reformulate("FRL_Percent", var), data = data_clean)
            p_value <- summary(model)$coefficients[2, 4]

            cat(var, "\n")
            cat("  Correlation:", round(cor_val, 3), "\n")
            cat("  P-value:", format.pval(p_value, digits = 3), "\n")
            cat("  Significance:", if (p_value < 0.05) "✓ Significant" else "Not significant", "\n\n")
        }
    }
}

# ============================================================================
# School-level Summary
# ============================================================================

cat("=== School-level Summary ===\n\n")

school_summary <- analysis_data %>%
    group_by(School, FRL_Percent) %>%
    summarize(
        N_Competitions = n(),
        Avg_Final_Score = mean(Final_Total, na.rm = TRUE),
        SD_Final_Score = sd(Final_Total, na.rm = TRUE),
        Avg_Music = mean(Music_Total, na.rm = TRUE),
        Avg_Visual = mean(Visual_Total, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(Avg_Final_Score))

cat("Top 10 Schools by Average Final Score:\n")
print(head(school_summary, 10), n = 10)

cat("\n\nBottom 10 Schools by Average Final Score:\n")
print(tail(school_summary, 10), n = 10)

# ============================================================================
# Visualizations
# ============================================================================

cat("\n\n=== Creating Visualizations ===\n\n")

# 1. Scatter plot: FRL vs Final Score
p1 <- ggplot(school_summary, aes(x = FRL_Percent, y = Avg_Final_Score)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
        title = "Band Performance vs Free/Reduced Lunch Percentage",
        subtitle = paste("Schools: ", n_distinct(school_summary$School)),
        x = "Free/Reduced Lunch Percentage (%)",
        y = "Average Final Score"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

ggsave("frl_vs_final_score.png", p1, width = 10, height = 6)
cat("✓ Saved: frl_vs_final_score.png\n")

# 2. Boxplot by FRL quartiles
school_summary <- school_summary %>%
    mutate(FRL_Quartile = cut(FRL_Percent,
                               breaks = quantile(FRL_Percent, probs = 0:4/4),
                               labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"),
                               include.lowest = TRUE))

p2 <- ggplot(school_summary, aes(x = FRL_Quartile, y = Avg_Final_Score, fill = FRL_Quartile)) +
    geom_boxplot(alpha = 0.7) +
    labs(
        title = "Band Performance by FRL Quartile",
        x = "Free/Reduced Lunch Quartile",
        y = "Average Final Score"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

ggsave("frl_quartiles_boxplot.png", p2, width = 10, height = 6)
cat("✓ Saved: frl_quartiles_boxplot.png\n")

# 3. Performance components by FRL level
component_data <- analysis_data %>%
    mutate(FRL_Level = cut(FRL_Percent,
                           breaks = c(0, 25, 50, 75, 100),
                           labels = c("0-25%", "26-50%", "51-75%", "76-100%"),
                           include.lowest = TRUE)) %>%
    group_by(FRL_Level) %>%
    summarize(
        Music = mean(Music_Total, na.rm = TRUE),
        Visual = mean(Visual_Total, na.rm = TRUE),
        Effect = mean(Music_Eff_Avg_Total, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    tidyr::pivot_longer(cols = c(Music, Visual, Effect),
                        names_to = "Component",
                        values_to = "Score")

p3 <- ggplot(component_data, aes(x = FRL_Level, y = Score, fill = Component)) +
    geom_col(position = "dodge") +
    labs(
        title = "Score Components by FRL Level",
        x = "Free/Reduced Lunch Percentage",
        y = "Average Score"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

ggsave("frl_components.png", p3, width = 10, height = 6)
cat("✓ Saved: frl_components.png\n")

# ============================================================================
# Export Results
# ============================================================================

cat("\n=== Exporting Results ===\n\n")

# Save school summary
write.csv(school_summary, "data/frl_school_summary.csv", row.names = FALSE)
cat("✓ Saved: data/frl_school_summary.csv\n")

# Create correlation report
cor_final <- cor(analysis_data$FRL_Percent, analysis_data$Final_Total, use = "complete.obs")
model_final <- lm(Final_Total ~ FRL_Percent, data = analysis_data)

sink("data/frl_correlation_report.txt")
cat("=== FRL and Band Performance Correlation Report ===\n")
cat("Generated:", format(Sys.time()), "\n\n")
cat("Sample Size:", nrow(analysis_data), "records\n")
cat("Schools:", n_distinct(analysis_data$School), "\n\n")
cat("Correlation (FRL% vs Final Score):", round(cor_final, 3), "\n\n")
cat("Linear Model Summary:\n")
print(summary(model_final))
cat("\n\nSchool Summary Statistics:\n")
print(summary(school_summary))
sink()

cat("✓ Saved: data/frl_correlation_report.txt\n\n")

cat("✓ Analysis complete!\n\n")
cat("Files created:\n")
cat("  - frl_vs_final_score.png (scatter plot)\n")
cat("  - frl_quartiles_boxplot.png (boxplot by quartiles)\n")
cat("  - frl_components.png (performance components)\n")
cat("  - data/frl_school_summary.csv (school-level data)\n")
cat("  - data/frl_correlation_report.txt (statistical report)\n")
