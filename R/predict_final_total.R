library(tidymodels)
library(dplyr)

cat("=== Predicting Final_Total using tidymodels ===\n\n")

# Load validated band competition data
source('R/read_file.R')

# ============================================================================
# STEP 1: Prepare data
# ============================================================================

cat("Step 1: Preparing data\n")

# Select predictors and target
model_data <- scores_labeled_df_valid %>%
    select(
        Final_Total,
        Music_Ind_Tot,
        Music_Ens_Tot,
        Visual_Ind_Tot,
        Visual_Ens_Tot,
        Music_Eff_Avg_Total,
        Visual_Eff_Tot
    ) %>%
    # Remove any rows with missing values
    drop_na()

cat("  Total records:", nrow(model_data), "\n")
cat("  Predictors: Music_Ind_Tot, Music_Ens_Tot, Visual_Ind_Tot, Visual_Ens_Tot,\n")
cat("              Music_Eff_Avg_Total, Visual_Eff_Tot\n")
cat("  Target: Final_Total\n\n")

# ============================================================================
# STEP 2: Split data into training and testing
# ============================================================================

cat("Step 2: Splitting data (75% train / 25% test)\n")

set.seed(123)
data_split <- initial_split(model_data, prop = 0.75, strata = Final_Total)
train_data <- training(data_split)
test_data <- testing(data_split)

cat("  Training set:", nrow(train_data), "records\n")
cat("  Test set:", nrow(test_data), "records\n\n")

# ============================================================================
# STEP 3: Create recipe
# ============================================================================

cat("Step 3: Creating preprocessing recipe\n")

model_recipe <- recipe(Final_Total ~ ., data = train_data) %>%
    # Normalize all predictors
    step_normalize(all_predictors())

cat("  ✓ Recipe created with normalization\n\n")

# ============================================================================
# STEP 4: Specify models
# ============================================================================

cat("Step 4: Specifying models\n\n")

# Linear Regression
lm_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

# Random Forest
rf_spec <- rand_forest(trees = 500) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("regression")

# XGBoost
xgb_spec <- boost_tree(trees = 500) %>%
    set_engine("xgboost") %>%
    set_mode("regression")

cat("  Models specified:\n")
cat("    1. Linear Regression\n")
cat("    2. Random Forest (500 trees)\n")
cat("    3. XGBoost (500 trees)\n\n")

# ============================================================================
# STEP 5: Create workflows
# ============================================================================

cat("Step 5: Creating workflows\n")

lm_workflow <- workflow() %>%
    add_recipe(model_recipe) %>%
    add_model(lm_spec)

rf_workflow <- workflow() %>%
    add_recipe(model_recipe) %>%
    add_model(rf_spec)

xgb_workflow <- workflow() %>%
    add_recipe(model_recipe) %>%
    add_model(xgb_spec)

cat("  ✓ Workflows created\n\n")

# ============================================================================
# STEP 6: Fit models
# ============================================================================

cat("Step 6: Training models\n")

cat("  Training Linear Regression...\n")
lm_fit <- lm_workflow %>% fit(train_data)

cat("  Training Random Forest...\n")
rf_fit <- rf_workflow %>% fit(train_data)

cat("  Training XGBoost...\n")
xgb_fit <- xgb_workflow %>% fit(train_data)

cat("  ✓ All models trained\n\n")

# ============================================================================
# STEP 7: Make predictions
# ============================================================================

cat("Step 7: Making predictions on test set\n")

lm_pred <- lm_fit %>%
    predict(test_data) %>%
    bind_cols(test_data)

rf_pred <- rf_fit %>%
    predict(test_data) %>%
    bind_cols(test_data)

xgb_pred <- xgb_fit %>%
    predict(test_data) %>%
    bind_cols(test_data)

cat("  ✓ Predictions generated\n\n")

# ============================================================================
# STEP 8: Evaluate models
# ============================================================================

cat("=== Model Performance ===\n\n")

# Linear Regression metrics
lm_metrics <- lm_pred %>%
    metrics(truth = Final_Total, estimate = .pred)

cat("Linear Regression:\n")
print(lm_metrics)
cat("\n")

# Random Forest metrics
rf_metrics <- rf_pred %>%
    metrics(truth = Final_Total, estimate = .pred)

cat("Random Forest:\n")
print(rf_metrics)
cat("\n")

# XGBoost metrics
xgb_metrics <- xgb_pred %>%
    metrics(truth = Final_Total, estimate = .pred)

cat("XGBoost:\n")
print(xgb_metrics)
cat("\n")

# ============================================================================
# STEP 9: Compare models
# ============================================================================

cat("=== Model Comparison ===\n\n")

comparison <- bind_rows(
    lm_metrics %>% mutate(Model = "Linear Regression"),
    rf_metrics %>% mutate(Model = "Random Forest"),
    xgb_metrics %>% mutate(Model = "XGBoost")
) %>%
    select(Model, .metric, .estimate) %>%
    tidyr::pivot_wider(names_from = .metric, values_from = .estimate) %>%
    arrange(rmse)

print(comparison)
cat("\n")

# ============================================================================
# STEP 10: Feature importance (Random Forest)
# ============================================================================

cat("=== Feature Importance (Random Forest) ===\n\n")

rf_fit %>%
    extract_fit_parsnip() %>%
    vip::vip(num_features = 6)

# Extract importance values
importance_data <- rf_fit %>%
    extract_fit_engine() %>%
    vip::vi() %>%
    arrange(desc(Importance))

print(importance_data)
cat("\n")

# ============================================================================
# STEP 11: Linear regression coefficients
# ============================================================================

cat("=== Linear Regression Coefficients ===\n\n")

lm_coefs <- lm_fit %>%
    extract_fit_engine() %>%
    broom::tidy()

print(lm_coefs)
cat("\n")

# ============================================================================
# STEP 12: Cross-validation (best model)
# ============================================================================

cat("=== 10-Fold Cross-Validation (Random Forest) ===\n\n")

set.seed(123)
folds <- vfold_cv(train_data, v = 10, strata = Final_Total)

rf_cv_results <- rf_workflow %>%
    fit_resamples(
        resamples = folds,
        metrics = metric_set(rmse, rsq, mae)
    )

cv_metrics <- rf_cv_results %>%
    collect_metrics()

cat("Cross-validation results:\n")
print(cv_metrics)
cat("\n")

# ============================================================================
# STEP 13: Save results
# ============================================================================

cat("=== Saving Results ===\n\n")

# Save model comparison
write.csv(comparison, "data/model_comparison.csv", row.names = FALSE)
cat("✓ Saved: data/model_comparison.csv\n")

# Save feature importance
write.csv(importance_data, "data/feature_importance.csv", row.names = FALSE)
cat("✓ Saved: data/feature_importance.csv\n")

# Save predictions
write.csv(rf_pred, "data/predictions_rf.csv", row.names = FALSE)
cat("✓ Saved: data/predictions_rf.csv\n")

# Save linear model coefficients
write.csv(lm_coefs, "data/lm_coefficients.csv", row.names = FALSE)
cat("✓ Saved: data/lm_coefficients.csv\n\n")

# ============================================================================
# STEP 14: Visualizations
# ============================================================================

cat("=== Creating Visualizations ===\n\n")

library(ggplot2)

# 1. Predicted vs Actual (Random Forest)
p1 <- ggplot(rf_pred, aes(x = Final_Total, y = .pred)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
        title = "Random Forest: Predicted vs Actual Final_Total",
        x = "Actual Final_Total",
        y = "Predicted Final_Total"
    ) +
    theme_minimal()

ggsave("predicted_vs_actual_rf.png", p1, width = 8, height = 6)
cat("✓ Saved: predicted_vs_actual_rf.png\n")

# 2. Residuals plot
rf_pred <- rf_pred %>%
    mutate(residual = Final_Total - .pred)

p2 <- ggplot(rf_pred, aes(x = .pred, y = residual)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(
        title = "Random Forest: Residual Plot",
        x = "Predicted Final_Total",
        y = "Residual"
    ) +
    theme_minimal()

ggsave("residual_plot_rf.png", p2, width = 8, height = 6)
cat("✓ Saved: residual_plot_rf.png\n")

# 3. Feature importance plot
p3 <- ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Feature Importance (Random Forest)",
        x = "Feature",
        y = "Importance"
    ) +
    theme_minimal()

ggsave("feature_importance.png", p3, width = 8, height = 6)
cat("✓ Saved: feature_importance.png\n")

# 4. Model comparison plot
comparison_long <- comparison %>%
    tidyr::pivot_longer(cols = c(rmse, rsq, mae), names_to = "Metric", values_to = "Value")

p4 <- ggplot(comparison_long, aes(x = Model, y = Value, fill = Model)) +
    geom_col() +
    facet_wrap(~Metric, scales = "free_y") +
    labs(title = "Model Comparison") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

ggsave("model_comparison.png", p4, width = 10, height = 6)
cat("✓ Saved: model_comparison.png\n\n")

cat("✓ Analysis complete!\n\n")
cat("Summary:\n")
cat("  - Best model (by RMSE):", comparison$Model[1], "\n")
cat("  - Test RMSE:", round(comparison$rmse[1], 3), "\n")
cat("  - Test R²:", round(comparison$rsq[1], 3), "\n")
cat("  - Most important feature:", importance_data$Variable[1], "\n")
