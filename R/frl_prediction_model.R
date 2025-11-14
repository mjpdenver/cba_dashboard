library(tidymodels)
library(dplyr)

cat("=== Tidymodels: Predict Final_Total from Event_Class + FRL_Percent + School_Size ===\n\n")

# ============================================================================
# STEP 1: Load merged FRL data
# ============================================================================

cat("Step 1: Loading merged FRL data\n")

if (!file.exists("data/band_scores_with_frl.csv")) {
    cat("✗ Merged FRL data not found: data/band_scores_with_frl.csv\n")
    cat("\nPlease run the FRL merge first:\n")
    cat("  source('R/merge_frl_data.R')\n\n")
    stop("FRL data file not found")
}

scores_with_frl <- read.csv("data/band_scores_with_frl.csv", stringsAsFactors = FALSE)

cat("  Total records:", nrow(scores_with_frl), "\n")
cat("  Records with FRL data:", sum(!is.na(scores_with_frl$FRL_Percent)), "\n")
cat("  Records with School_Size:", sum(!is.na(scores_with_frl$School_Size)), "\n\n")

# ============================================================================
# STEP 2: Prepare data for modeling
# ============================================================================

cat("Step 2: Preparing data for modeling\n")

# Filter to complete cases (has all predictors and outcome)
model_data <- scores_with_frl %>%
    filter(!is.na(Final_Total),
           !is.na(Event_Class),
           !is.na(FRL_Percent),
           !is.na(School_Size)) %>%
    select(Final_Total, Event_Class, FRL_Percent, School_Size) %>%
    mutate(Event_Class = factor(Event_Class))

cat("  Complete cases for modeling:", nrow(model_data), "\n")
cat("  Event Class distribution:\n")
print(table(model_data$Event_Class))
cat("\n")

if (nrow(model_data) < 30) {
    cat("⚠ Warning: Small sample size (n =", nrow(model_data), ")\n")
    cat("  Model results may not be reliable\n\n")
}

# ============================================================================
# STEP 3: Split data
# ============================================================================

cat("Step 3: Splitting data (75% train / 25% test)\n")

set.seed(123)
data_split <- initial_split(model_data, prop = 0.75, strata = Event_Class)
train_data <- training(data_split)
test_data <- testing(data_split)

cat("  Training set:", nrow(train_data), "observations\n")
cat("  Test set:", nrow(test_data), "observations\n\n")

# ============================================================================
# STEP 4: Create model specification and recipe
# ============================================================================

cat("Step 4: Building model specification\n")

# Linear regression model
lm_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

# Recipe: predict Final_Total from Event_Class + FRL_Percent + School_Size
model_recipe <- recipe(Final_Total ~ Event_Class + FRL_Percent + School_Size,
                       data = train_data) %>%
    step_dummy(Event_Class)  # Convert Event_Class to dummy variables

# Create workflow
model_workflow <- workflow() %>%
    add_model(lm_spec) %>%
    add_recipe(model_recipe)

cat("  Model: Linear Regression\n")
cat("  Predictors: Event_Class, FRL_Percent, School_Size\n")
cat("  Outcome: Final_Total\n\n")

# ============================================================================
# STEP 5: Train model
# ============================================================================

cat("Step 5: Training model\n")

fitted_model <- model_workflow %>%
    fit(data = train_data)

cat("✓ Model trained successfully\n\n")

# ============================================================================
# STEP 6: Evaluate on test set
# ============================================================================

cat("Step 6: Evaluating model performance\n\n")

# Predictions on test set
test_predictions <- fitted_model %>%
    predict(new_data = test_data) %>%
    bind_cols(test_data)

# Calculate metrics
test_metrics <- test_predictions %>%
    metrics(truth = Final_Total, estimate = .pred)

cat("Test Set Performance:\n")
print(test_metrics)
cat("\n")

# Calculate R-squared manually for clarity
test_rsq <- cor(test_predictions$Final_Total, test_predictions$.pred)^2
cat("Test R²:", round(test_rsq, 4), "\n")
cat("Test RMSE:", round(test_metrics %>% filter(.metric == "rmse") %>% pull(.estimate), 2), "\n\n")

# ============================================================================
# STEP 7: Show model coefficients
# ============================================================================

cat("Step 7: Model coefficients\n\n")

# Extract fitted model
fitted_lm <- fitted_model %>%
    extract_fit_engine()

# Show summary
cat("Regression Summary:\n")
cat(strrep("=", 70), "\n\n")
print(summary(fitted_lm))
cat("\n")

# ============================================================================
# STEP 8: Predictions on full dataset
# ============================================================================

cat("Step 8: Predictions on full training data\n\n")

# Predictions on training set
train_predictions <- fitted_model %>%
    predict(new_data = train_data) %>%
    bind_cols(train_data)

# Calculate training metrics
train_metrics <- train_predictions %>%
    metrics(truth = Final_Total, estimate = .pred)

cat("Training Set Performance:\n")
print(train_metrics)
cat("\n")

train_rsq <- cor(train_predictions$Final_Total, train_predictions$.pred)^2
cat("Training R²:", round(train_rsq, 4), "\n")
cat("Training RMSE:", round(train_metrics %>% filter(.metric == "rmse") %>% pull(.estimate), 2), "\n\n")

# ============================================================================
# STEP 9: Variable importance (coefficient magnitude)
# ============================================================================

cat("Step 9: Variable importance (standardized coefficients)\n\n")

# Get coefficients
coefs <- tidy(fitted_lm) %>%
    filter(term != "(Intercept)") %>%
    arrange(desc(abs(estimate)))

cat("Coefficients (sorted by absolute value):\n")
print(coefs, n = Inf)
cat("\n")

# ============================================================================
# STEP 10: Example predictions
# ============================================================================

cat("Step 10: Example predictions\n\n")

# Create example scenarios
examples <- tibble(
    Event_Class = factor(c("3A", "3A", "4A", "4A"), levels = levels(model_data$Event_Class)),
    FRL_Percent = c(20, 60, 20, 60),
    School_Size = c(500, 500, 1500, 1500)
)

example_preds <- fitted_model %>%
    predict(new_data = examples) %>%
    bind_cols(examples)

cat("Predicted Final_Total for different scenarios:\n")
print(example_preds)
cat("\n")

# ============================================================================
# Summary
# ============================================================================

cat("=== Summary ===\n\n")
cat("Model: Final_Total ~ Event_Class + FRL_Percent + School_Size\n")
cat("Training R²:", round(train_rsq, 4), "\n")
cat("Test R²:", round(test_rsq, 4), "\n")
cat("Test RMSE:", round(test_metrics %>% filter(.metric == "rmse") %>% pull(.estimate), 2), "\n\n")

if (test_rsq < 0.1) {
    cat("⚠ Low R²: Event_Class, FRL_Percent, and School_Size explain little variance\n")
    cat("  Consider adding more predictors (e.g., Music_Total, Visual_Total, Effect scores)\n\n")
} else if (test_rsq > 0.5) {
    cat("✓ Good model fit: These predictors explain", round(test_rsq * 100, 1), "% of variance\n\n")
} else {
    cat("○ Moderate model fit: These predictors explain", round(test_rsq * 100, 1), "% of variance\n\n")
}

cat("✓ Analysis complete!\n")
