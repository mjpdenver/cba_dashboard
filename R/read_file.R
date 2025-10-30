library(pdftools)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

# Define data directory and PDF files
data_dir <- "data"
pdf_files <- c("2025 3A Finals.pdf", "2025 3A Semi Finals.pdf")

extract_scores_labeled <- function(file, directory = data_dir) {
    # Construct full path
    full_path <- file.path(directory, file)
    text <- pdf_text(full_path) |> paste(collapse = "\n")
    lines <- str_split(text, "\n")[[1]] |> str_squish()

    # Extract event date - look for common date patterns
    event_date <- NA
    date_patterns <- c(
        "\\d{1,2}/\\d{1,2}/\\d{4}",      # MM/DD/YYYY or M/D/YYYY
        "\\d{4}-\\d{2}-\\d{2}",           # YYYY-MM-DD
        "[A-Za-z]+ \\d{1,2},? \\d{4}"    # Month DD, YYYY or Month DD YYYY
    )
    for (pattern in date_patterns) {
        match <- str_extract(text, pattern)
        if (!is.na(match)) {
            event_date <- match
            break
        }
    }

    # Lines that contain a school name (ending in HS followed by numbers)
    school_lines <- lines[str_detect(lines, "(?i)HS\\s+\\d")]
    
    df_list <- map(school_lines, function(line) {
        school <- str_extract(line, "^[A-Za-z'&\\- ]+HS")

        # Extract all numeric values (including negative signs for penalties)
        nums_raw <- str_extract_all(line, "-?\\d+\\.?\\d*")[[1]]

        # Keep only decimal values (summary scores) - these are the totals, not individual judge scores
        # Also keep the last value which should be Final_Total (may be integer or decimal)
        is_decimal <- str_detect(nums_raw, "\\.")
        decimal_nums <- as.numeric(nums_raw[is_decimal])

        # If we have more than 13 decimals, take the last 13 (most reliable summary values)
        if (length(decimal_nums) > 13) {
            decimal_nums <- tail(decimal_nums, 13)
        }

        nums <- decimal_nums
        
        # Assign labels — based on expected order from PDFs
        labels <- c(
            "Music_Individual",
            "Music_Ensemble",
            "Music_Total",
            "Visual_Individual",
            "Visual_Ensemble",
            "Visual_Total",
            "Music_Effect1",
            "Music_Effect2",
            "Visual_Effect",
            "Sub_Total",
            "Weighted_Total",
            "Timing_Penalties",
            "Final_Total"
        )
        
        # Only label as many columns as exist
        names(nums) <- labels[seq_along(nums)]
        
        tibble(
            File = file,
            Event_Date = event_date,
            School = school,
            !!!as.list(nums)
        )
    })
    
    bind_rows(df_list)
}

# Apply to both files and combine
scores_labeled_df <- map(pdf_files, extract_scores_labeled) |> bind_rows()

# Clean up
scores_labeled_df <- scores_labeled_df |> arrange(File, School)

# View result
print(scores_labeled_df)
