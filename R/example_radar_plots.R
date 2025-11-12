# Example: Create radar plots for multiple schools
library(fmsb)
library(dplyr)

# Load the data
source('R/read_file.R')

# Load the radar plot function
source('R/radar_plot.R')

# Create radar plots for several schools and save them

cat("Creating radar plots for multiple schools...\n\n")

# School 1: Wheat Ridge
cat("1. Wheat Ridge\n")
create_school_radar("Wheat Ridge", save_file = "plots/wheat_ridge_radar.png")

# School 2: Palisade HS
cat("\n2. Palisade HS\n")
# Check if school has multiple competitions
palisade_data <- scores_labeled_df %>% filter(School == "Palisade HS")
if (nrow(palisade_data) > 1) {
    cat("Multiple competitions found. Creating plot for most recent...\n")
    latest_date <- max(palisade_data$Event_Date)
    create_school_radar("Palisade HS", competition_date = latest_date,
                       save_file = "plots/palisade_radar.png")
} else if (nrow(palisade_data) == 1) {
    create_school_radar("Palisade HS", save_file = "plots/palisade_radar.png")
}

# School 3: Pueblo County HS
cat("\n3. Pueblo County HS\n")
create_school_radar("Pueblo County HS")

, save_file = "plots/pueblo_county_radar.png")

cat("\nAll plots created successfully!\n")
cat("Check the 'plots/' directory for the saved PNG files.\n")
