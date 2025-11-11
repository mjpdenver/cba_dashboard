# Workflow: Download, Validate, and Combine Data from URLs

This guide explains how to download PDF competition data from URLs, validate it, and combine it with your existing dataset.

## Two Approaches

### Approach A: Download from Base URLs (Recommended)

Use this when you have web pages that list multiple PDFs (like the coloradomarching.org scores pages).

**Script:** `R/download_and_combine_from_urls.R`

This approach:
1. Scrapes PDF links from each base_url webpage
2. Downloads all PDFs found on those pages
3. Extracts, validates, and combines data

### Approach B: Download Direct PDF URLs

Use this when you have direct links to specific PDF files.

**Script:** `R/download_and_combine.R`

## Quick Start (Approach A - Base URLs)

### Step 1: Update Base URLs

Edit `R/download_and_combine_from_urls.R` (lines 16-20) with your base URLs:

```r
base_urls <- c(
    "https://www.coloradomarching.org/copy-of-scores",
    "https://www.coloradomarching.org/2023-scores",
    "https://www.coloradomarching.org/2022-scores"
)
```

### Step 2: Run the Workflow

```r
source('R/download_and_combine_from_urls.R')
```

## Quick Start (Approach B - Direct URLs)

### Step 1: Update PDF URLs

Edit `R/download_and_combine.R` (lines 16-20) with direct PDF URLs:

```r
pdf_urls <- c(
    "https://example.com/competition1.pdf",
    "https://example.com/competition2.pdf",
    "https://example.com/competition3.pdf"
)
```

### Step 2: Run the Workflow

```r
source('R/download_and_combine.R')
```

### Step 3: Review Results

The script creates:
- `combined_scores_df` - Combined dataset in memory
- `data/combined_scores.csv` - Saved combined data
- `data/download_validation_report.txt` - Validation summary
- `data/downloads/` - Downloaded PDF files

## Which Approach Should I Use?

**Use Approach A (Base URLs)** when:
- You have web pages that list multiple PDFs (e.g., https://www.coloradomarching.org/copy-of-scores)
- You want to download ALL PDFs from those pages automatically
- The website structure is similar to coloradomarching.org (HTML page with PDF links)
- **Example:** Your existing `download_pdf.R` uses this approach

**Use Approach B (Direct URLs)** when:
- You have direct links to specific PDF files
- You want to download only certain PDFs, not all from a page
- The PDFs are hosted on different sites without a common listing page

## Detailed Workflow (Approach A - Base URLs)

### Phase 1: Scrape and Download (Automated)

The script automatically:
1. Scrapes each base_url webpage for PDF links
2. Extracts all links ending in `.pdf`
3. Creates `data/downloads/` directory if needed
4. Downloads each PDF (skips files that already exist)
5. Reports success/failure for each download

**Output:**
```
STEP 1: Scraping PDF links from base URLs
==========================================

Scraping: https://www.coloradomarching.org/copy-of-scores
  ✓ Found 47 PDF links

✓ Total unique PDF links found: 47

STEP 2: Downloading PDFs
========================
  ↓ Downloading: competition1.pdf
    ✓ Success
  ○ competition2.pdf already exists — skipping

✓ Download Summary:
  Total links: 47
  Successfully downloaded: 35
  Already existed: 12
  Failed: 0
```

### Phase 2: Extraction (Automated)

Uses your existing `extract_scores_labeled()` function to:
1. Parse each downloaded PDF
2. Extract school names, scores, and metadata
3. Combine into a single dataframe

**Output:**
```
STEP 3: Extracting data from PDFs
==================================
Processing: competition1.pdf
  ✓ Extracted 45 school records

✓ Extraction Summary:
  Files processed: 47
  Files with data: 45
  Extraction errors: 2
  Total records extracted: 1,247
```

### Phase 3: Validation (Automated)

Applies validation rules:
- Fields ending in `_Tot`: must be 0-20
- `Music_Total`, `Visual_Total`: must be 0-20
- Identifies invalid records
- Automatically filters to valid records only

**Output:**
```
STEP 4: Validating new data
============================
New records: 1,247
Invalid records: 8
Valid records: 1,239
```

### Phase 4: Combine (Automated)

1. Loads existing `scores_labeled_df` from `read_file.R`
2. Checks for duplicate records (same school, competition, date)
3. Combines existing + new valid records
4. Reports summary statistics

**Output:**
```
STEP 5: Combining with existing data
=====================================
Existing records: 404
New valid records: 1,239
✓ Combined dataset created
  Total records: 1,643
  Total schools: 156
  Total competitions: 89
  Date range: 2022-09-10 to 2024-10-26
```

### Phase 5: Save (Automated)

Saves three files:
1. **data/combined_scores.csv** - Full combined dataset
2. **data/downloaded_pdf_links.txt** - List of all PDF URLs scraped
3. **data/download_validation_report.txt** - Summary report

## Alternative Workflows

### Option A: Manual Download + Automatic Processing

If you already have PDFs downloaded:

```r
# 1. Place PDFs in data/downloads/
# 2. Run extraction only
source('R/read_file.R')
source('R/filter_valid_scores.R')

# 3. Use the filtered data
analysis_data <- scores_labeled_df_valid
```

### Option B: Interactive Step-by-Step

```r
# Step 1: Download manually
download.file("URL1", "data/downloads/comp1.pdf", mode = "wb")
download.file("URL2", "data/downloads/comp2.pdf", mode = "wb")
download.file("URL3", "data/downloads/comp3.pdf", mode = "wb")

# Step 2: Extract
source('R/read_file.R')

# Step 3: Validate
source('R/filter_valid_scores.R')

# Step 4: Check results
cat("Valid records:", nrow(scores_labeled_df_valid), "\n")
summary(scores_labeled_df_valid)

# Step 5: Save if satisfied
write.csv(scores_labeled_df_valid, "data/my_clean_data.csv", row.names = FALSE)
```

### Option C: Update Existing Data Directory

If you want to integrate downloaded PDFs into your existing workflow:

```r
# Download to your main data directory
pdf_urls <- c("URL1", "URL2", "URL3")

for (i in seq_along(pdf_urls)) {
    filename <- paste0("new_comp_", i, ".pdf")
    download.file(pdf_urls[i], file.path("data", filename), mode = "wb")
}

# Then run your normal workflow
source('R/read_file.R')
source('R/filter_valid_scores.R')
```

## Handling Duplicates

The workflow warns about duplicates but doesn't automatically remove them. To remove duplicates:

```r
# After running download_and_combine.R
combined_scores_df_unique <- combined_scores_df %>%
    distinct(School, Competition_Name, Event_Date, .keep_all = TRUE)

cat("Original:", nrow(combined_scores_df), "\n")
cat("After removing duplicates:", nrow(combined_scores_df_unique), "\n")
```

## Handling Invalid Records

The workflow automatically excludes invalid records. To review what was excluded:

```r
# After running download_and_combine.R
# Check the validation report
file.show("data/download_validation_report.txt")

# Or examine invalid records manually
source('R/test_score_validation.R')  # Shows detailed validation results
```

## Integration with Existing Scripts

After downloading and combining, you can use any of your existing analysis scripts:

```r
# 1. Download and combine
source('R/download_and_combine.R')

# 2. Run event summary
scores_labeled_df <- combined_scores_df  # Use combined data
source('R/event_summary.R')

# 3. Create visualizations
source('R/correlation_matrix.R')
source('R/radar_plot_ggplot.R')
```

## Troubleshooting

### Downloads Fail
- Check internet connection
- Verify URLs are accessible in a browser
- Some sites may block automated downloads (use manual download instead)

### No Data Extracted
- Verify PDFs have the expected format (similar to existing files)
- Check that school names contain "HS" or match the pattern
- Run `pdf_text("downloaded_file.pdf")` to inspect PDF structure

### Validation Failures
- Review `data/download_validation_report.txt`
- Run `source('R/test_score_validation.R')` for detailed issues
- Check if PDFs have different scoresheet layouts

### Duplicates Found
- This is expected if competitions overlap between old and new data
- Use `.distinct()` to remove duplicates (see "Handling Duplicates" above)

## Comparison: Approach A vs B

| Feature | Approach A (Base URLs) | Approach B (Direct URLs) |
|---------|------------------------|--------------------------|
| **Script** | `download_and_combine_from_urls.R` | `download_and_combine.R` |
| **Input** | Web pages listing PDFs | Direct PDF URLs |
| **Scraping** | ✓ Automatic scraping | ✗ Manual URL collection |
| **Scalability** | High (gets all PDFs on page) | Low (must specify each URL) |
| **Use Case** | Download entire archives | Download specific files |
| **Requires** | rvest, httr packages | httr package only |
| **Like existing** | `download_pdf.R` | New approach |

## File Locations

```
cba_dashboard/
├── R/
│   ├── download_pdf.R                      # Original scraper (single base_url)
│   ├── download_and_combine_from_urls.R    # New: Multiple base_urls + combine
│   ├── download_and_combine.R              # Alternative: Direct URLs + combine
│   ├── download_example.R                  # Usage examples
│   └── DOWNLOAD_WORKFLOW.md                # This guide
├── data/
│   ├── downloads/                          # Downloaded PDFs (auto-created)
│   ├── combined_scores.csv                 # Combined output
│   ├── downloaded_pdf_links.txt            # List of PDF URLs found
│   └── download_validation_report.txt      # Validation summary
```
