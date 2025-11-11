library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data
source('R/read_file.R')

# Function to create a multi-school comparison radar plot
compare_schools_radar <- function(school_names, event_class = NULL, save_file = NULL) {

    # Filter data for selected schools
    schools_data <- scores_labeled_df %>%
        filter(School %in% school_names)

    # Filter by event class if specified
    if (!is.null(event_class)) {
        schools_data <- schools_data %>% filter(Event_Class == event_class)
    }

    # For schools with multiple entries, take the most recent
    schools_data <- schools_data %>%
        group_by(School) %>%
        arrange(desc(Event_Date)) %>%
        slice(1) %>%
        ungroup()

    if (nrow(schools_data) == 0) {
        stop("No data found for specified schools")
    }

    cat("Comparing", nrow(schools_data), "schools:\n")
    print(schools_data[, c('School', 'Competition_Name', 'Event_Date', 'Final_Total')])
    cat("\n")

    # Prepare data for radar chart
    plot_data <- schools_data %>%
        select(School, Music_Total, Visual_Total, Visual_Eff_Tot) %>%
        pivot_longer(cols = c(Music_Total, Visual_Total, Visual_Eff_Tot),
                    names_to = "Metric",
                    values_to = "Value") %>%
        mutate(Metric = case_when(
            Metric == "Music_Total" ~ "Music",
            Metric == "Visual_Total" ~ "Visual",
            Metric == "Visual_Eff_Tot" ~ "Visual\nEffect"
        ))

    # Duplicate first row for each school to close polygon
    plot_data <- plot_data %>%
        group_by(School) %>%
        arrange(Metric) %>%
        do({
            rbind(., .[1, ])
        }) %>%
        ungroup()

    # Create color palette
    n_schools <- length(unique(plot_data$School))
    colors <- scales::hue_pal()(n_schools)

    # Create the comparison radar plot
    p <- ggplot(plot_data, aes(x = Metric, y = Value, group = School, color = School, fill = School)) +
        # Add filled polygons
        geom_polygon(alpha = 0.3, linewidth = 1.2) +
        # Add points
        geom_point(size = 3) +
        # Convert to polar coordinates
        coord_polar() +
        # Set y-axis limits
        ylim(0, 20) +
        # Labels and theme
        labs(
            title = "School Performance Comparison",
            subtitle = paste("Comparing", n_schools, "schools across key metrics"),
            y = "Score",
            color = "School",
            fill = "School"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 11, face = "bold"),
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            panel.grid.major = element_line(color = "gray80")
        )

    # Display or save
    if (!is.null(save_file)) {
        ggsave(save_file, plot = p, width = 10, height = 10, dpi = 300)
        cat("Comparison plot saved to:", save_file, "\n")
    } else {
        print(p)
    }

    invisible(p)
}

# ===== Example Usage =====

# Example 1: Compare top 3A schools
cat("Example 1: Comparing top 3A schools...\n\n")
top_3a_schools <- scores_labeled_df %>%
    filter(Event_Class == "3A", !is.na(Final_Total)) %>%
    arrange(desc(Final_Total)) %>%
    head(5) %>%
    pull(School) %>%
    unique()

compare_schools_radar(top_3a_schools, save_file = "comparison_top_3a.png")

# Example 2: Compare specific schools
# compare_schools_radar(c("Wheat Ridge", "Palisade HS", "Pueblo County HS"),
#                      save_file = "comparison_selected.png")

# Example 3: Compare schools from same competition
# same_competition <- scores_labeled_df %>%
#     filter(Competition_Name == "Legend Invitational") %>%
#     arrange(desc(Final_Total)) %>%
#     head(4) %>%
#     pull(School)
# compare_schools_radar(same_competition, save_file = "comparison_legend.png")
