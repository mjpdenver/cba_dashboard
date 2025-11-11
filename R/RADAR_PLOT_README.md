# Radar Plot Visualization for CBA Scores

## Overview

The `radar_plot.R` script creates radar/star plots to visualize three key performance metrics for any school:
- **Music Total**: Overall music performance score
- **Visual Total**: Overall visual performance score
- **Visual Effect Total**: Visual effect performance score

## Requirements

Install the required package:
```r
install.packages('fmsb')
```

## Usage

### Basic Usage

```r
# Load the radar plot script
source('R/radar_plot.R')

# Create a radar plot for a specific school
create_school_radar("Wheat Ridge")
```

### Save Plot to File

```r
# Save the plot as a PNG file
create_school_radar("Wheat Ridge", save_file = "wheat_ridge_radar.png")
```

### Handle Multiple Competitions

If a school has participated in multiple competitions, you can specify which one:

```r
# Filter by event class
create_school_radar("Palisade HS", event_class = "3A")

# Filter by competition date
create_school_radar("D'Evelyn HS", competition_date = as.Date("2024-10-28"))

# Save filtered result
create_school_radar("Palisade HS",
                   event_class = "3A",
                   save_file = "palisade_3A.png")
```

## Function Parameters

### `create_school_radar(school_name, competition_date, event_class, save_file)`

- **school_name** (required): Exact school name as it appears in the data (e.g., "Wheat Ridge", "Palisade HS")
- **competition_date** (optional): Date object to filter specific competition
- **event_class** (optional): Event class to filter (e.g., "1A", "2A", "3A", "4A")
- **save_file** (optional): File path to save the plot (e.g., "my_plot.png")

## Output

The radar plot shows:
- Three axes representing Music, Visual, and Visual Effect scores
- Scale from 0 to 20 (typical score range)
- Blue shaded polygon showing the school's performance
- Title showing school name, competition, and date

## Example: Multiple Schools

```r
# Create plots for comparison
create_school_radar("Wheat Ridge", save_file = "plots/wheat_ridge.png")
create_school_radar("Palisade HS", save_file = "plots/palisade.png")
create_school_radar("Pueblo County HS", save_file = "plots/pueblo_county.png")
```

## Finding Available Schools

To see all available schools:
```r
source('R/read_file.R')
sort(unique(scores_labeled_df$School))
```

## Tips

1. Use exact school names (case-sensitive)
2. For schools in multiple competitions, check which competitions are available:
   ```r
   scores_labeled_df %>%
     filter(School == "Your School Name") %>%
     select(Competition_Name, Event_Date, Event_Class, Final_Total)
   ```
3. Save plots to a dedicated folder (e.g., `plots/`) for better organization
