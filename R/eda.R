
source('R/filter_valid_scores.R')


library(ggplot2)

tmp <- scores_labeled_df %>% filter(!is.na(Final_Total) & Visual_Total <=20)

tmp <- scores_labeled_df_valid

valid_scores <- scores_labeled_df %>%
    filter(
        (is.na(Visual_Ind_Tot) | Visual_Ind_Tot <= 20) &
            (is.na(Music_Eff1_Tot) | Music_Eff1_Tot <= 20) &
            (is.na(Music_Eff2_Tot) | Music_Eff2_Tot <= 20) &
            (is.na(Visual_Eff_Tot) | Visual_Eff_Tot <= 20) &
            (is.na(Music_Total) | Music_Total <= 20) &
            (is.na(Visual_Total) | Visual_Total <= 20)
    )

p <- ggplot(tmp, aes(Music_Ind_Mus, Music_Ind_TAT))
p + geom_point() + geom_abline()

p <- ggplot(tmp, aes(Music_Ens_Mus, Music_Ens_TAT))
p + geom_point() + geom_abline()

p <- ggplot(tmp, aes(Music_Ens_Tot, Music_Ind_Tot))
p + geom_point(aes(col = Event_Type)) + geom_abline()

p <- ggplot(tmp, aes(Music_Total, Visual_Total))
p + geom_point(aes(col = Event_Type)) + geom_abline()

### which variables have the most variation
library(dplyr)

tmp <- scores_labeled_df_valid %>%
    select(where(is.numeric) & !ends_with(c("_Tot", "_Total", "Penalty")))

tmp %>% pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    ggplot(aes(x = reorder(variable, value, FUN = median), y = value)) +
    geom_boxplot() + coord_flip()

p <- ggplot(scores_labeled_df_valid, aes(x = Event_Class, y = Final_Total) )
p + geom_boxplot()

### consider ratios

rep_cols <- grep("_REP$", names(scores_labeled_df_valid), value = TRUE)

# Extract base names (e.g., "Music_Eff1" from "Music_Eff1_REP")
base_names <- sub("_REP$", "", rep_cols)

# Create ratio columns
df <- scores_labeled_df_valid %>%
    mutate(across(
        .cols = all_of(rep_cols),
        .fns = ~ . / get(paste0(cur_column() %>% sub("_REP$", "", .), "_PRF")),
        .names = "{sub('_REP$', '_Ratio', .col)}"
    ))

df <- scores_labeled_df_valid
# Get all _REP columns and create ratios
rep_cols <- grep("_REP$", names(df), value = TRUE)
df[paste0(sub("_REP$", "_Ratio", rep_cols))] <- 
    lapply(rep_cols, function(x) {
        df[[x]] / df[[sub("_REP$", "_PRF", x)]]
    })

# After creating ratios, reshape to long format
rep_cols <- grep("_REP$", names(df), value = TRUE)
ratio_cols <- paste0(sub("_REP$", "_Ratio", rep_cols))

df_long <- df %>%
    pivot_longer(cols = all_of(ratio_cols),
                 names_to = "Category",
                 values_to = "Ratio") %>%
    mutate(Category = sub("_Ratio$", "", Category))

# Create boxplot
ggplot(df_long, aes(x = Category, y = Ratio, fill = Category)) +
    geom_boxplot() +
    labs(title = "Distribution of REP/PRF Ratios",
         x = "Effect Category",
         y = "Ratio (REP/PRF)") +
    theme_minimal() +
    theme(legend.position = "none")

tmp <- scores_labeled_df_valid %>% 
    filter( Competition_Name == "CBA 2A-4A Metro Regional 2024" &
                Event_Class == "3A") %>% 
    select(School, ends_with("_Tot"), ends_with("_Total") ) 
    

tmp <- scores_labeled_df_valid %>% 
    filter( Competition_Name == "CBA 2A-4A Metro Regional 2024" &
                Event_Class == "3A") %>% 
    select(School, Music_Total, Visual_Total, Visual_Eff_Tot, Music_Eff_Avg_Total)

tmp_long <- tmp %>%
    pivot_longer(cols = c(Music_Total, Visual_Total, Visual_Eff_Tot, Music_Eff_Avg_Total),
                 names_to = "Category",
                 values_to = "Score")

# Vertical facets (categories as rows)
ggplot(tmp_long, aes(x = School, y = Score, fill = Category)) +
    geom_col() +
    facet_grid(Category ~ ., scales = "free_y") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")

# OR Horizontal facets (categories as columns)
ggplot(tmp_long, aes(x = School, y = Score, fill = Category)) +
    geom_col() +
    facet_grid(. ~ Category) + coord_flip() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none")
