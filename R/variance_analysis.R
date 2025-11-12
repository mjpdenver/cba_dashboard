library(dplyr)
library(ggplot2)
library(tidyr)

cat("=== Variance Contribution Analysis ===\n\n")

# Load validated band competition data
source('R/read_file.R')

# ============================================================================
# STEP 1: Calculate variance of each predictor
# ============================================================================

cat("Step 1: Variance of each predictor\n\n")

predictors <- c("Music_Ind_Tot", "Music_Ens_Tot", "Visual_Ind_Tot",
                "Visual_Ens_Tot", "Music_Eff_Avg_Total", "Visual_Eff_Tot")

variance_table <- scores_labeled_df_valid %>%
    select(all_of(predictors), Final_Total) %>%
    drop_na() %>%
    summarise(across(everything(), list(
        mean = ~mean(.),
        sd = ~sd(.),
        var = ~var(.),
        min = ~min(.),
        max = ~max(.)
    ))) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    separate(metric, into = c("variable", "stat"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    arrange(desc(var))

cat("Variance statistics:\n")
print(variance_table, n = Inf)
cat("\n")

# ============================================================================
# STEP 2: Correlation with Final_Total
# ============================================================================

cat("Step 2: Correlation with Final_Total\n\n")

model_data <- scores_labeled_df_valid %>%
    select(all_of(predictors), Final_Total) %>%
    drop_na()

correlations <- sapply(predictors, function(pred) {
    cor(model_data[[pred]], model_data$Final_Total)
})

cor_table <- data.frame(
    Predictor = predictors,
    Correlation = correlations
) %>%
    arrange(desc(abs(Correlation)))

print(cor_table)
cat("\n")

# ============================================================================
# STEP 3: Linear regression to get coefficients
# ============================================================================

cat("Step 3: Regression coefficients (how much each unit change affects Final_Total)\n\n")

model <- lm(Final_Total ~ Music_Ind_Tot + Music_Ens_Tot + Visual_Ind_Tot +
            Visual_Ens_Tot + Music_Eff_Avg_Total + Visual_Eff_Tot,
            data = model_data)

coefs <- summary(model)$coefficients[-1, c("Estimate", "Std. Error", "t value")]
coef_table <- data.frame(
    Predictor = predictors,
    Coefficient = coefs[, "Estimate"],
    Std_Error = coefs[, "Std. Error"],
    t_value = coefs[, "t value"]
) %>%
    arrange(desc(abs(Coefficient)))

print(coef_table)
cat("\n")

# ============================================================================
# STEP 4: Variance contribution (variance × coefficient²)
# ============================================================================

cat("Step 4: Variance contribution to Final_Total\n\n")
cat("This shows which predictor contributes most to the variance in Final_Total\n")
cat("Formula: Contribution = Variance × Coefficient²\n\n")

# Merge variance and coefficient info
contribution_table <- variance_table %>%
    filter(variable %in% predictors) %>%
    left_join(coef_table, by = c("variable" = "Predictor")) %>%
    mutate(
        Variance_Contribution = var * Coefficient^2,
        Percent_Contribution = 100 * Variance_Contribution / sum(Variance_Contribution)
    ) %>%
    arrange(desc(Variance_Contribution)) %>%
    select(variable, mean, sd, var, Coefficient, Variance_Contribution, Percent_Contribution)

print(contribution_table, n = Inf)
cat("\n")

# ============================================================================
# STEP 5: Standardized coefficients (Beta weights)
# ============================================================================

cat("Step 5: Standardized coefficients (Beta weights)\n\n")
cat("Shows impact when all predictors are on the same scale (z-scores)\n\n")

# Standardize all variables
model_data_std <- model_data %>%
    mutate(across(everything(), ~(. - mean(.)) / sd(.)))

model_std <- lm(Final_Total ~ Music_Ind_Tot + Music_Ens_Tot + Visual_Ind_Tot +
                Visual_Ens_Tot + Music_Eff_Avg_Total + Visual_Eff_Tot,
                data = model_data_std)

beta_weights <- coef(model_std)[-1]
beta_table <- data.frame(
    Predictor = predictors,
    Beta_Weight = beta_weights,
    Abs_Beta = abs(beta_weights)
) %>%
    arrange(desc(Abs_Beta))

print(beta_table)
cat("\n")

# ============================================================================
# STEP 6: Summary and interpretation
# ============================================================================

cat("=== SUMMARY ===\n\n")

cat("1. HIGHEST VARIANCE (most spread in scores):\n")
cat("   ", contribution_table$variable[1], "- Variance =",
    round(contribution_table$var[1], 2), "\n\n")

cat("2. LARGEST COEFFICIENT (biggest multiplier effect):\n")
cat("   ", coef_table$Predictor[1], "- Coefficient =",
    round(coef_table$Coefficient[1], 3), "\n\n")

cat("3. LARGEST VARIANCE CONTRIBUTION (explains most variation in Final_Total):\n")
cat("   ", contribution_table$variable[1], "-",
    round(contribution_table$Percent_Contribution[1], 1), "% of variance\n\n")

cat("4. HIGHEST STANDARDIZED IMPACT (when all scaled equally):\n")
cat("   ", beta_table$Predictor[1], "- Beta =",
    round(beta_table$Beta_Weight[1], 3), "\n\n")

cat("INTERPRETATION:\n")
cat("The predictor with the highest variance contribution (",
    contribution_table$variable[1], ") is the most important\n")
cat("for explaining differences in Final_Total across schools.\n")
cat("Even though all predictors together perfectly determine Final_Total,\n")
cat("this predictor accounts for", round(contribution_table$Percent_Contribution[1], 1),
    "% of the variation.\n\n")

# ============================================================================
# STEP 7: Visualizations
# ============================================================================

cat("=== Creating Visualizations ===\n\n")

# 1. Variance comparison
p1 <- ggplot(variance_table %>% filter(variable != "Final_Total"),
             aes(x = reorder(variable, var), y = var, fill = variable)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Variance of Each Predictor",
        x = "Predictor",
        y = "Variance"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("variance_by_predictor.png", p1, width = 10, height = 6)
cat("✓ Saved: variance_by_predictor.png\n")

# 2. Coefficient comparison
p2 <- ggplot(coef_table, aes(x = reorder(Predictor, Coefficient),
                              y = Coefficient, fill = Predictor)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Regression Coefficients (Impact per Unit)",
        x = "Predictor",
        y = "Coefficient"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("coefficients_by_predictor.png", p2, width = 10, height = 6)
cat("✓ Saved: coefficients_by_predictor.png\n")

# 3. Variance contribution
p3 <- ggplot(contribution_table,
             aes(x = reorder(variable, Percent_Contribution),
                 y = Percent_Contribution, fill = variable)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = paste0(round(Percent_Contribution, 1), "%")),
              hjust = -0.1, size = 3) +
    labs(
        title = "Variance Contribution to Final_Total",
        subtitle = "Which predictor explains the most variation in final scores?",
        x = "Predictor",
        y = "% of Total Variance Explained"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(0, max(contribution_table$Percent_Contribution) * 1.15)

ggsave("variance_contribution.png", p3, width = 10, height = 6)
cat("✓ Saved: variance_contribution.png\n")

# 4. Beta weights
p4 <- ggplot(beta_table, aes(x = reorder(Predictor, Abs_Beta),
                              y = Beta_Weight, fill = Predictor)) +
    geom_col() +
    coord_flip() +
    labs(
        title = "Standardized Coefficients (Beta Weights)",
        subtitle = "Impact when all predictors are scaled to same units",
        x = "Predictor",
        y = "Beta Weight"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("beta_weights.png", p4, width = 10, height = 6)
cat("✓ Saved: beta_weights.png\n")

# 5. Scatterplots showing relationship with Final_Total
scatter_data <- model_data %>%
    select(all_of(predictors), Final_Total) %>%
    pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value")

p5 <- ggplot(scatter_data, aes(x = Value, y = Final_Total)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    facet_wrap(~Predictor, scales = "free_x", ncol = 3) +
    labs(
        title = "Relationship Between Each Predictor and Final_Total",
        x = "Predictor Value",
        y = "Final Total Score"
    ) +
    theme_minimal()

ggsave("predictor_scatterplots.png", p5, width = 12, height = 8)
cat("✓ Saved: predictor_scatterplots.png\n\n")

# ============================================================================
# STEP 8: Save results
# ============================================================================

cat("=== Saving Results ===\n\n")

write.csv(variance_table, "data/variance_statistics.csv", row.names = FALSE)
cat("✓ Saved: data/variance_statistics.csv\n")

write.csv(contribution_table, "data/variance_contribution.csv", row.names = FALSE)
cat("✓ Saved: data/variance_contribution.csv\n")

write.csv(beta_table, "data/beta_weights.csv", row.names = FALSE)
cat("✓ Saved: data/beta_weights.csv\n\n")

cat("✓ Analysis complete!\n")
