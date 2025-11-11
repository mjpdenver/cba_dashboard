# Create a correlation matrix heatmap using ggplot2
library(dplyr)
library(ggplot2)
library(tidyr)

#source('R/read_file.R')

source("R/filter_valid_scores.R")
cat("=== Creating Correlation Matrix ===\n\n")

# Filter to valid records only
component_scores_tot <- c("Music_Ind_Tot", "Music_Ens_Tot",
                          "Visual_Ind_Tot", "Visual_Ens_Tot",
                          "Music_Eff1_Tot", "Music_Eff2_Tot", "Visual_Eff_Tot")
component_scores_total <- c("Music_Total", "Visual_Total")
all_validated_fields <- c(component_scores_tot, component_scores_total)

scores_valid <- scores_labeled_df_valid %>%
    filter(if_all(all_of(all_validated_fields), ~ is.na(.) | (. >= 0 & . <= 20)))

cat("Using", nrow(scores_valid), "valid records\n\n")

# Select key score variables for correlation matrix
score_vars <- c(
    "Music_Ind_Tot", "Music_Ens_Tot", "Music_Total",
    "Visual_Ind_Tot", "Visual_Ens_Tot", "Visual_Total",
    "Music_Eff1_Tot", "Music_Eff2_Tot", "Visual_Eff_Tot",
    "General_Effect_Total",
    "Sub_Total", "Weighted_Total", "Final_Total"
)

# Calculate correlation matrix
cor_data <- scores_valid %>%
    select(all_of(score_vars)) %>%
    cor(use = "complete.obs")

# Convert to long format for ggplot
cor_long <- cor_data %>%
    as.data.frame() %>%
    mutate(Var1 = rownames(.)) %>%
    pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

# Create ordered factor for proper ordering in plot
var_order <- score_vars
cor_long <- cor_long %>%
    mutate(
        Var1 = factor(Var1, levels = var_order),
        Var2 = factor(Var2, levels = rev(var_order))
    )

# Create the correlation heatmap
p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Correlation)),
              color = "black", size = 2.5) +
    scale_fill_gradient(
        low = "#FEE0D2",
        high = "#A50F15",
        limit = c(0.8, 1.0),
        name = "Correlation"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5)
    ) +
    labs(
        title = "Correlation Matrix of Band Competition Scores",
        subtitle = paste0("Based on ", nrow(scores_valid), " valid competition records")
    ) +
    coord_fixed()

# Display the plot
print(p)

# Save the plot
ggsave("correlation_matrix.png", plot = p, width = 10, height = 8, dpi = 300)
cat("\n✓ Plot saved as: correlation_matrix.png\n")

# Print summary statistics
cat("\n=== Strongest Correlations (excluding self-correlations) ===\n")
strong_cors <- cor_long %>%
    filter(Var1 != Var2) %>%
    arrange(desc(abs(Correlation))) %>%
    head(10)

print(strong_cors, n = 10)
