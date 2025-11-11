library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data
source('R/read_file.R')

# Function to create a radar plot using ggplot2
create_school_radar_ggplot <- function(school_name, competition_date = NULL, event_class = NULL, save_file = NULL) {

    # Filter data for the selected school
    school_data <- scores_labeled_df %>%
        filter(School == school_name)

    # If multiple competitions, filter by date or class if provided
    if (!is.null(competition_date)) {
        school_data <- school_data %>% filter(Event_Date == competition_date)
    }
    if (!is.null(event_class)) {
        school_data <- school_data %>% filter(Event_Class == event_class)
    }

    # If still multiple rows, show options and take the first one
    if (nrow(school_data) > 1) {
        cat("Multiple competitions found for", school_name, ". Using first entry.\n")
        cat("Available competitions:\n")
        print(school_data[, c('Competition_Name', 'Event_Date', 'Event_Class', 'Final_Total')])
        school_data <- school_data[1, ]
    }

    if (nrow(school_data) == 0) {
        stop("No data found for school: ", school_name)
    }

    # Extract the three variables
    music_total <- school_data$Music_Total
    visual_total <- school_data$Visual_Total
    visual_effect_total <- school_data$Visual_Eff_Tot

    cat("\nSchool:", school_name, "\n")
    cat("Competition:", school_data$Competition_Name, "\n")
    cat("Date:", as.character(school_data$Event_Date), "\n")
    cat("Music Total:", music_total, "\n")
    cat("Visual Total:", visual_total, "\n")
    cat("Visual Effect Total:", visual_effect_total, "\n\n")

    # Prepare data for ggplot2 radar chart
    plot_data <- data.frame(
        Metric = c("Music", "Visual", "Visual\nEffect"),
        Value = c(music_total, visual_total, visual_effect_total)
    )

    # Add a duplicate of the first row to close the polygon
    plot_data <- rbind(plot_data, plot_data[1, ])

    # Create the radar plot
    p <- ggplot(plot_data, aes(x = Metric, y = Value, group = 1)) +
        # Add filled polygon
        geom_polygon(fill = alpha("#3182BD", 0.5), color = "#3182BD", linewidth = 1.5) +
        # Add points at each vertex
        geom_point(color = "#3182BD", size = 4) +
        # Convert to polar coordinates
        coord_polar() +
        # Set y-axis limits (0-20 scale)
        ylim(0, 20) +
        # Add labels and theme
        labs(
            title = school_name,
            subtitle = paste(school_data$Competition_Name, "-", school_data$Event_Date),
            y = "Score"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 10),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray90")
        )

    # Display or save the plot
    if (!is.null(save_file)) {
        ggsave(save_file, plot = p, width = 8, height = 8, dpi = 300)
        cat("Plot saved to:", save_file, "\n")
    } else {
        print(p)
    }

    # Return the plot object
    invisible(p)
}

# ===== Example Usage =====

# Example 1: Create radar plot for a specific school (displays on screen)
cat("Example 1: Creating ggplot2 radar plot for Wheat Ridge...\n")
create_school_radar_ggplot("Wheat Ridge")

# Example 2: Save plot to a file
# create_school_radar_ggplot("Wheat Ridge", save_file = "wheat_ridge_radar_ggplot.png")

# Example 3: For schools with multiple competitions, specify date or class
# create_school_radar_ggplot("Palisade HS", event_class = "3A")
# create_school_radar_ggplot("D'Evelyn HS", competition_date = as.Date("2024-10-28"))

# List all available schools
cat("\n\nAvailable schools:\n")
schools <- unique(scores_labeled_df$School)
print(sort(schools))
