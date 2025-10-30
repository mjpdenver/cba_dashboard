library(pdftools)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

# Define data directory and PDF files
data_dir <- "data"
pdf_files <- c("2025 3A Finals.pdf", "2025 3A Semi Finals.pdf", "test.pdf")

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

    # Extract event type (Regional, State, etc.)
    event_type <- NA
    event_type_match <- str_extract(text, "(?i)(Regional|State)")
    if (!is.na(event_type_match)) {
        event_type <- str_to_title(event_type_match)
    }

    # Extract event round (Finals, Semi Finals, Quarterfinals)
    event_round <- NA
    round_match <- str_extract(text, "(?i)(Quarter[- ]?Finals?|Semi[- ]?Finals?|Finals?)")
    if (!is.na(round_match)) {
        # Normalize the format
        round_match <- str_to_title(round_match)
        round_match <- str_replace_all(round_match, "\\s*-\\s*", " ")  # Normalize hyphens to spaces
        round_match <- str_replace(round_match, "(?i)^Finals?$", "Finals")
        round_match <- str_replace(round_match, "(?i)Semi.*Finals?", "Semi Finals")
        round_match <- str_replace(round_match, "(?i)Quarter.*Finals?", "Quarterfinals")
        event_round <- round_match
    }

    # Lines that contain a school name (ending in HS followed by numbers)
    # Filter out header lines that might contain "HS" or other patterns
    potential_school_lines <- lines[str_detect(lines, "(?i)HS\\s+\\d")]

    # Remove lines that look like headers (contain words like "Performance", "Effect", "Individual", etc.)
    header_keywords <- c("Performance", "Individual", "Ensemble", "Effect", "Visual", "Music", "Timing", "Penalties", "Judge", "Total")
    school_lines <- potential_school_lines[!str_detect(potential_school_lines, paste(header_keywords, collapse = "|"))]

    # Track the class for each school by finding "Class XA" headers
    # Build a mapping of line numbers to classes
    class_map <- rep(NA_character_, length(lines))
    current_class <- NA_character_
    for (i in seq_along(lines)) {
        # Check if this line is a class header
        class_match <- str_extract(lines[i], "(?i)Class\\s+[1-4]A")
        if (!is.na(class_match)) {
            current_class <- str_extract(class_match, "[1-4]A")
        }
        class_map[i] <- current_class
    }
    
    df_list <- map(school_lines, function(line) {
        school <- str_extract(line, "^[A-Za-z'&\\- ]+HS")

        # Find which line this is and get its class
        line_idx <- which(lines == line)[1]
        event_class <- class_map[line_idx]

        # Extract all numeric values (including negative signs for penalties)
        nums_raw <- str_extract_all(line, "-?\\d+\\.?\\d*")[[1]]
        nums <- as.numeric(nums_raw)

        # Filter out ranking numbers (single digit integers at the end of sequences)
        # Typically these are 1-10 and appear after score values
        keep <- rep(TRUE, length(nums))
        for (i in seq_along(nums)) {
            # If it's a single-digit integer between 1-10, it's likely a ranking
            if (!is.na(nums[i]) && nums[i] == floor(nums[i]) && nums[i] >= 1 && nums[i] <= 10) {
                # Check if it's isolated (not part of a larger score like 10.5)
                if (i == length(nums) || (i < length(nums) && nums[i+1] < 20)) {
                    keep[i] <- FALSE
                }
            }
        }
        nums <- nums[keep]

        # Ensure we have exactly 28 values
        if (length(nums) < 28) {
            nums <- c(nums, rep(NA, 28 - length(nums)))
        } else if (length(nums) > 28) {
            nums <- nums[1:28]
        }
        
        # Assign labels for all 28 values — based on expected order from PDFs
        labels <- c(
            "Music_Ind_Mus",
            "Music_Ind_TAT",
            "Music_Ind_Tot",
            "Music_Ens_Mus",
            "Music_Ens_TAT",
            "Music_Ens_Tot",
            "Music_Total",
            "Visual_Ind_ChCnt",
            "Visual_Ind_Ach",
            "Visual_Ind_Tot",
            "Visual_Ens_CMP",
            "Visual_Ens_ACH",
            "Visual_Ens_Tot",
            "Visual_Total",
            "Music_Eff1_REP",
            "Music_Eff1_PRF",
            "Music_Eff1_Tot",
            "Music_Eff2_REP",
            "Music_Eff2_PRF",
            "Music_Eff2_Tot",
            "Visual_Eff_REP",
            "Visual_Eff_PRF",
            "Visual_Eff_Tot",
            "Sub_Total",
            "Weighted_Total",
            "Timing_Penalty",
            "Timing_Penalty_Tot",
            "Final_Total"
        )
        
        # Only label as many columns as exist
        names(nums) <- labels[seq_along(nums)]
        
        tibble(
            File = file,
            Event_Date = event_date,
            Event_Type = event_type,
            Event_Round = event_round,
            Event_Class = event_class,
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
