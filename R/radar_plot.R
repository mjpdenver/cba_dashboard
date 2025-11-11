library(fmsb)
library(dplyr)

# Load the data
source('R/read_file.R')

# Function to create a radar plot for a selected school
create_school_radar <- function(school_name, competition_date = NULL, event_class = NULL, save_file = NULL) {

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

    # Create data frame for radar chart
    # fmsb requires first row = max, second row = min, third row = data
    # Scale is typically 0-20 for these totals
    max_val <- 20
    min_val <- 0

    radar_data <- data.frame(
        Music = c(max_val, min_val, music_total),
        Visual = c(max_val, min_val, visual_total),
        Visual_Effect = c(max_val, min_val, visual_effect_total)
    )

    # Save to file if specified
    if (!is.null(save_file)) {
        png(save_file, width = 800, height = 800)
    }

    # Create the radar chart
    radarchart(
        radar_data,
        axistype = 1,
        # Polygon customization
        pcol = rgb(0.2, 0.5, 0.8, 0.9),
        pfcol = rgb(0.2, 0.5, 0.8, 0.5),
        plwd = 2,
        # Grid customization
        cglcol = "grey",
        cglty = 1,
        axislabcol = "grey",
        caxislabels = seq(0, max_val, 5),
        cglwd = 0.8,
        # Label customization
        vlcex = 1.2,
        title = paste(school_name, "\n",
                     school_data$Competition_Name, "-", school_data$Event_Date)
    )

    if (!is.null(save_file)) {
        dev.off()
        cat("Plot saved to:", save_file, "\n")
    }

    # Return the data for reference
    invisible(school_data)
}

# ===== Example Usage =====

# Example 1: Create radar plot for a specific school (displays on screen)
cat("Example 1: Creating radar plot for Wheat Ridge...\n")
create_school_radar("Wheat Ridge")

# Example 2: Save plot to a file
# create_school_radar("Wheat Ridge", save_file = "wheat_ridge_radar.png")

# Example 3: For schools with multiple competitions, specify date or class
# create_school_radar("Palisade HS", event_class = "3A")
# create_school_radar("D'Evelyn HS", competition_date = as.Date("2024-10-28"))

# List all available schools
cat("\n\nAvailable schools:\n")
schools <- unique(scores_labeled_df$School)
print(sort(schools))
