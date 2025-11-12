# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Architectual Principals

**Simplicity First**: Keep the codebase as small and simple as possible


## Project Overview

This is an R project for analyzing competition band assessment (CBA) data. The primary focus is extracting, processing, and visualizing band competition scores from PDF files.

## Development Environment

This is an RStudio project. Open `cba_dashboard.Rproj` in RStudio to work with proper project settings:
- 4 spaces for indentation
- UTF-8 encoding
- Code indexing enabled

## Core Dependencies

Install required packages before running scripts:

```r
# For read_file.R (PDF extraction)
install.packages(c("pdftools", "stringr", "dplyr", "purrr", "tidyr", "lubridate"))

# For radar_plot.R (fmsb-based radar charts)
install.packages("fmsb")

# For radar_plot_ggplot.R and compare_schools_radar.R (ggplot2-based radar charts)
install.packages(c("ggplot2", "scales"))

# For tem.R (ternary plotting examples)
install.packages(c("tidymodels", "plotrix", "ggtern", "Ternary", "modeldata"))
```

## Key Files

### R/read_file.R
The main PDF extraction script for processing competition scoresheets:
- Processes multiple PDF files from a specified directory (default: `CBA_scores_pdfs/`)
- Uses tidyverse pipe operators (`|>`) and `purrr::map()` for functional processing
- Automatically detects schools with or without "HS" suffix (e.g., "Wheat Ridge" or "Palisade HS")
- Extracts comprehensive metadata:
  - **Competition_Name**: Full competition name (e.g., "Legend Invitational")
  - **Event_Date**: Date as Date class (parsed from multiple formats)
  - **Event_Type**: Regional, State, or Invitational (auto-classified)
  - **Event_Round**: Prelims, Finals, Semi Finals, or Quarterfinals
  - **Event_Class**: 1A, 2A, 3A, or 4A
- Extracts all 28 numeric score values per school:
  - **Music Performance**: Individual (Mus, T-A-T, Tot), Ensemble (Mus, T-A-T, Tot), Total
  - **Visual Performance**: Individual (Ch-Cnt, Ach, Tot), Ensemble (CMP, ACH, Tot), Total
  - **General Effect**: Music Effect 1 (REP, PRF, Tot), Music Effect 2 (REP, PRF, Tot), Visual Effect (REP, PRF, Tot)
  - **Totals**: Sub_Total, Weighted_Total, Timing_Penalty, Timing_Penalty_Tot, Final_Total
- Filters out ranking numbers and header lines automatically
- Outputs `scores_labeled_df` with 36 columns total (including calculated General_Effect_Total)

**Data Location**: Reads PDFs from directory specified in `data_dir` variable (line 8). Uses `dir()` to auto-detect all PDF files.

**Column Structure**: Complete 36-column structure
- Metadata: `File`, `Competition_Name`, `Event_Date`, `Event_Type`, `Event_Round`, `Event_Class`, `School`
- 28 score columns with detailed breakdowns
- 1 calculated field: `General_Effect_Total` (sum of Music_Eff1_Tot + Music_Eff2_Tot + Visual_Eff_Tot)

### R/radar_plot.R
Creates radar/star plots using the `fmsb` package (traditional approach):
- Simple, specialized radar chart generation
- Visualizes Music_Total, Visual_Total, and Visual_Eff_Tot on a 0-20 scale
- Supports filtering by competition date or event class
- Best for quick, standalone visualizations

**Example Usage**:
```r
source('R/radar_plot.R')
create_school_radar("Wheat Ridge", save_file = "wheat_ridge.png")
```

### R/radar_plot_ggplot.R
Creates radar/star plots using `ggplot2` (modern, customizable approach):
- Full ggplot2 integration with extensive customization options
- Returns plot object that can be further modified
- Better quality output with ggsave
- Supports themes (theme_minimal, theme_dark, etc.)
- Ideal for publication-quality graphics and tidyverse workflows

**Example Usage**:
```r
source('R/radar_plot_ggplot.R')
p <- create_school_radar_ggplot("Wheat Ridge", save_file = "wheat_ridge.png")
p + theme_dark()  # Further customize
```

### R/compare_schools_radar.R
Creates multi-school comparison radar plots using ggplot2:
- Overlays multiple schools on one radar chart
- Automatically selects most recent competition for each school
- Color-coded by school with legend
- Perfect for comparing top performers or schools from same competition

**Example Usage**:
```r
source('R/compare_schools_radar.R')
compare_schools_radar(c("Wheat Ridge", "Palisade HS", "D'Evelyn HS"))
```

**See Also**: `R/RADAR_PLOT_COMPARISON.md` for detailed comparison of both approaches

### R/test_tot_variables.R & R/test_score_validation.R
Data validation scripts to ensure score integrity:
- **test_tot_variables.R**: Tests all variables ending in "_Tot" or "_Total" for expected ranges
- **test_score_validation.R**: Categorized validation (component scores 0-20, aggregates can exceed 20, penalties can be negative)
- Results documented in `R/VALIDATION_RESULTS.md`
- Current status: 95-96% of schools have valid data; ~15-20 schools have extraction issues

**Run validation**:
```r
source('R/test_score_validation.R')
```

### R/event_summary.R
Generates comprehensive summaries of all competitions in the dataset:
- Breakdown by Event Type (Invitational, Regional, State)
- Breakdown by Event Round (Prelims, Finals, Semi Finals, Quarterfinals)
- School counts per competition
- Automatically filters invalid school names

### R/tem.R
Experimental ternary plotting examples using the Ames housing dataset. Contains examples for three different ternary plot packages (plotrix, ggtern, Ternary).

## Common Workflows

### Analyzing New Competition Data

1. Place the PDF scoresheet(s) in the `data/` directory
2. Update the `pdf_files` vector in `R/read_file.R` (line 8) to include the new filename(s)
   ```r
   pdf_files <- c("2025 3A Finals.pdf", "2025 3A Semi Finals.pdf", "new_file.pdf")
   ```
3. Run the script - it automatically detects all schools and combines results
4. The output dataframe includes a `File` column to distinguish between competitions

### Running Scripts

Execute in RStudio:
```r
source("R/read_file.R")
```

Or from command line:
```bash
Rscript R/read_file.R
```

### Saving Results

Add export commands at the end of `read_file.R`:
```r
write.csv(scores_labeled_df, "output/competition_results.csv", row.names = FALSE)
```

## Architecture Notes

### PDF Extraction Pipeline (read_file.R)

The `extract_scores_labeled()` function processes each PDF:
1. **Text Extraction** → `pdf_text()` reads and collapses pages
2. **Line Splitting** → Text split into individual lines and whitespace normalized
3. **School Detection** → Regex pattern `(?i)HS\\s+\\d` identifies school data rows
4. **Numeric Parsing** → Extracts all numbers, then filters out ranking integers (≤10)
5. **Labeling** → Maps remaining numbers to predefined score column names
6. **Combining** → `map()` + `bind_rows()` merges all schools from all files

**Important**: The extraction assumes consistent PDF formatting. School names must end in "HS" followed by numeric scores. Changes to PDF layout require updating the regex patterns in lines 15-18.

### Data Model

Competition scores follow this structure:
- **Performance Scores**: Music (Individual, Ensemble, Total) and Visual (Individual, Ensemble, Total)
- **Effect Scores**: Music_Effect1, Music_Effect2, Visual_Effect
- **Aggregations**: Sub_Total, Weighted_Total, Timing_Penalties, Final_Total

Each row represents one school from one competition file. The `File` column allows analysis across multiple competitions.
