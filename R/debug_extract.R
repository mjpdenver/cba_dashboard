library(pdftools)
library(stringr)

# Extract first school line and show all numbers
text <- pdf_text("data/2025 3A Finals.pdf") |> paste(collapse = "\n")
lines <- str_split(text, "\n")[[1]] |> str_squish()
school_lines <- lines[str_detect(lines, "(?i)HS\\s+\\d")]

cat("First school line:\n")
cat(school_lines[1], "\n\n")

# Extract all numbers
nums_raw <- str_extract_all(school_lines[1], "\\d+\\.?\\d*")[[1]]
cat("All extracted numbers (", length(nums_raw), " total):\n", sep="")
for (i in seq_along(nums_raw)) {
    cat(sprintf("%2d: %s\n", i, nums_raw[i]))
}

# Apply current filtering logic
nums <- as.numeric(nums_raw)
is_decimal <- str_detect(nums_raw, "\\.")
keep <- rep(TRUE, length(nums))
for (i in seq_along(nums)[-1]) {
    if (!is_decimal[i] && nums[i] <= 10 && !is.na(nums[i - 1])) {
        keep[i] <- FALSE
    }
}

cat("\n\nAfter filtering (", sum(keep), " remaining):\n", sep="")
filtered_nums <- nums[keep]
for (i in seq_along(filtered_nums)) {
    cat(sprintf("%2d: %s\n", i, filtered_nums[i]))
}
