library(pdftools)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

# Define data directory and PDF files
# Handle running from both project root and R/ subdirectory
if (dir.exists("CBA_scores_pdfs2/")) {
    data_dir <- "CBA_scores_pdfs2/"
} else if (dir.exists("../CBA_scores_pdfs2/")) {
    data_dir <- "../CBA_scores_pdfs2/"
} else {
    stop("Cannot find CBA_scores_pdfs2/ directory. Please run from project root or R/ subdirectory.")
}

pdf_files <- dir(data_dir, pattern = "*.pdf")

extract_scores_labeled <- function(file, directory = data_dir) {
    # Construct full path
    full_path <- file.path(directory, file)
    text <- pdf_text(full_path) |> paste(collapse = "\n")
    lines <- str_split(text, "\n")[[1]] |> str_squish()

    # Extract event date - look for full date format first (e.g., "Saturday, September 28, 2024")
    # This helps find the main date, not footer dates
    event_date <- NA
    date_line_idx <- NA

    # Try to find full date with day of week first (most reliable for finding main date)
    full_date_pattern <- "(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday),?\\s+[A-Za-z]+\\s+\\d{1,2},?\\s+\\d{4}"
    full_date_match <- str_extract(text, full_date_pattern)

    if (!is.na(full_date_match)) {
        # Extract just the date part (without day of week)
        event_date <- str_extract(full_date_match, "[A-Za-z]+\\s+\\d{1,2},?\\s+\\d{4}")
        # Find which line contains this full date
        date_line_idx <- which(str_detect(lines, fixed(full_date_match)))[1]
    } else {
        # Fall back to simpler date patterns
        date_patterns <- c(
            "[A-Za-z]+ \\d{1,2},? \\d{4}",  # Month DD, YYYY (try this first)
            "\\d{1,2}/\\d{1,2}/\\d{4}",      # MM/DD/YYYY or M/D/YYYY
            "\\d{4}-\\d{2}-\\d{2}"            # YYYY-MM-DD
        )
        for (pattern in date_patterns) {
            match <- str_extract(text, pattern)
            if (!is.na(match)) {
                event_date <- match
                # Find which line contains this date (exclude lines with "of" to avoid footer dates)
                matching_lines <- which(str_detect(lines, fixed(match)))
                # Filter out lines that look like footers (contain "of" like "1 of 3")
                non_footer_lines <- matching_lines[!str_detect(lines[matching_lines], "\\d+\\s+of\\s+\\d+")]
                if (length(non_footer_lines) > 0) {
                    date_line_idx <- non_footer_lines[1]
                } else {
                    date_line_idx <- matching_lines[1]
                }
                break
            }
        }
    }

    # Extract competition name - typically 2 lines above the date
    competition_name <- NA
    if (!is.na(date_line_idx) && date_line_idx > 2) {
        # Look 2 lines above the date
        comp_line <- lines[date_line_idx - 2]
        # Clean up the line - remove empty strings and extra whitespace
        if (!is.na(comp_line) && nchar(comp_line) > 0) {
            competition_name <- comp_line
        }
    }

    # Extract event type (Regional, State, or Invitational)
    event_type <- NA
    event_type_match <- str_extract(text, "(?i)(Regional|State)")
    if (!is.na(event_type_match)) {
        event_type <- str_to_title(event_type_match)
    } else {
        # If not Regional or State, classify as Invitational
        event_type <- "Invitational"
    }

    # Extract event round (Prelims, Finals, Semi Finals, Quarterfinals)
    event_round <- NA
    # Check for Prelims first (common in invitationals)
    prelims_match <- str_extract(text, "(?i)Prelims?")
    if (!is.na(prelims_match)) {
        event_round <- "Prelims"
    } else {
        # Check for Finals, Semi Finals, Quarterfinals
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
    }

    # Lines that contain a school name followed by numbers
    # Match patterns: "School Name HS" or just "School Name" followed by numbers
    # Pattern matches: capitalized words (including spaces, hyphens, apostrophes) followed by either "HS" or just whitespace, then digits
    potential_school_lines <- lines[str_detect(lines, "^[A-Z][A-Za-z'&\\-]+(\\s+[A-Z][A-Za-z'&\\-]+)*(\\s+HS)?\\s+\\d")]

    # Remove lines that look like headers (contain words like "Performance", "Effect", "Individual", etc.)
    # Also filter out lines that start with "Class" (class headers)
    header_keywords <- c("Performance", "Individual", "Ensemble", "Effect", "Visual", "Music", "Timing", "Penalties", "Judge", "Total", "Head", "Sub", "Weighted")
    school_lines <- potential_school_lines[!str_detect(potential_school_lines, paste(header_keywords, collapse = "|"))]
    school_lines <- school_lines[!str_detect(school_lines, "^Class\\s+[1-4]A")]

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
        # Extract school name - either ending in "HS" or just the capitalized words before numbers
        school <- str_extract(line, "^[A-Z][A-Za-z'&\\-]+(\\s+[A-Z][A-Za-z'&\\-]+)*(\\s+HS)?")
        school <- str_trim(school)

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
            Competition_Name = competition_name,
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

# Convert Event_Date to Date class
# First ensure Event_Date is character type
scores_labeled_df <- scores_labeled_df |>
    mutate(Event_Date_char = as.character(Event_Date))

# Then convert based on format
scores_labeled_df <- scores_labeled_df |>
    mutate(Event_Date = case_when(
        # MM/DD/YYYY format
        str_detect(Event_Date_char, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ mdy(Event_Date_char),
        # YYYY-MM-DD format
        str_detect(Event_Date_char, "^\\d{4}-\\d{2}-\\d{2}$") ~ ymd(Event_Date_char),
        # Month DD, YYYY format
        str_detect(Event_Date_char, "^[A-Za-z]+ \\d{1,2},? \\d{4}$") ~ mdy(Event_Date_char),
        # Default - return NA
        TRUE ~ as.Date(NA)
    )) |>
    select(-Event_Date_char)  # Remove temporary column

# Clean up - filter out invalid school names and arrange
scores_labeled_df <- scores_labeled_df |>
    filter(!str_detect(School, "^(CBA|Class)")) |>
    # Calculate General_Effect_Total (sum of three effect scores)
    mutate(General_Effect_Total = Music_Eff1_Tot + Music_Eff2_Tot + Visual_Eff_Tot) |>
    mutate(Music_Eff_Avg_Total = (Music_Eff1_Tot + Music_Eff2_Tot)/2 ) |>
    arrange(File, School)

# Create validated dataset - filter out records with scores outside expected ranges
scores_labeled_df_valid <- scores_labeled_df |>
    filter(
        # Component scores (_Tot fields) must be 0-20
        (is.na(Music_Ind_Tot) | (Music_Ind_Tot >= 0 & Music_Ind_Tot <= 20)) &
        (is.na(Music_Ens_Tot) | (Music_Ens_Tot >= 0 & Music_Ens_Tot <= 20)) &
        (is.na(Visual_Ind_Tot) | (Visual_Ind_Tot >= 0 & Visual_Ind_Tot <= 20)) &
        (is.na(Visual_Ens_Tot) | (Visual_Ens_Tot >= 0 & Visual_Ens_Tot <= 20)) &
        (is.na(Music_Eff1_Tot) | (Music_Eff1_Tot >= 0 & Music_Eff1_Tot <= 20)) &
        (is.na(Music_Eff2_Tot) | (Music_Eff2_Tot >= 0 & Music_Eff2_Tot <= 20)) &
        (is.na(Visual_Eff_Tot) | (Visual_Eff_Tot >= 0 & Visual_Eff_Tot <= 20)) &
        # Music_Total and Visual_Total must be 0-20
        (is.na(Music_Total) | (Music_Total >= 0 & Music_Total <= 20)) &
        (is.na(Visual_Total) | (Visual_Total >= 0 & Visual_Total <= 20)) &
        # Timing_Penalty_Tot must be -10 to 0
        (is.na(Timing_Penalty_Tot) | (Timing_Penalty_Tot >= -10 & Timing_Penalty_Tot <= 0))
    )

# Report validation results
cat("\n=== Data Validation Summary ===\n")
cat("Total records extracted:", nrow(scores_labeled_df), "\n")
cat("Valid records:", nrow(scores_labeled_df_valid), "\n")
cat("Invalid records filtered out:", nrow(scores_labeled_df) - nrow(scores_labeled_df_valid), "\n")

if (nrow(scores_labeled_df) > nrow(scores_labeled_df_valid)) {
    cat("\n⚠ Warning: Some records have out-of-range scores (likely PDF extraction issues)\n")
    invalid_schools <- scores_labeled_df |>
        anti_join(scores_labeled_df_valid, by = c("File", "School", "Event_Date")) |>
        select(School, Competition_Name, Event_Date) |>
        distinct() |>
        arrange(School)
    cat("Schools with invalid data:\n")
    print(invalid_schools, n = Inf)
    cat("\nUse 'scores_labeled_df_valid' for analysis (recommended)\n")
    cat("Use 'scores_labeled_df' to see all data including invalid records\n")
} else {
    cat("✓ All records are valid\n")
}

cat("\n")

# View result
print(scores_labeled_df_valid)
