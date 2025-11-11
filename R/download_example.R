# Example: Download and combine data from 3 URLs
# This is a template - update the URLs with your actual PDF links

library(dplyr)

# Source the workflow script
source('R/download_and_combine.R')

# EXAMPLE USAGE:
# ==============

# Option 1: Edit the pdf_urls in download_and_combine.R directly
# Replace lines 16-20 with your actual URLs:
#
# pdf_urls <- c(
#     "https://coloradobands.org/results/competition1.pdf",
#     "https://coloradobands.org/results/competition2.pdf",
#     "https://coloradobands.org/results/competition3.pdf"
# )

# Option 2: Run interactively
# If you want to run this step-by-step, you can execute each section:

# 1. Download PDFs
# download.file("URL1", "data/downloads/comp1.pdf", mode = "wb")
# download.file("URL2", "data/downloads/comp2.pdf", mode = "wb")
# download.file("URL3", "data/downloads/comp3.pdf", mode = "wb")

# 2. Then run the existing extraction
# source('R/read_file.R')  # This will process all PDFs in data/

# 3. Validate
# source('R/filter_valid_scores.R')

# 4. Use the filtered data
# my_analysis_data <- scores_labeled_df_valid
