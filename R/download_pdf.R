### download files
library(rvest)
library(httr)
library(stringr)
library(purrr)

# URL which lists the PDFs
base_url <- "https://www.coloradomarching.org/copy-of-scores"

# directory to store the downloaded PDFs
out_dir <- "CBA_scores_pdfs"
if (!dir.exists(out_dir)) {
    dir.create(out_dir)
}

# read the page
page <- read_html(base_url)

# extract all links that end in “.pdf”
pdf_links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    # ensure full URLs
    map_chr(~ ifelse(str_starts(.x, "http"), .x, url_absolute(.x, base_url))) %>%
    # filter for pdf files
    keep(~ str_detect(.x, "(?i)\\.pdf$"))

# show what we found
message("Found ", length(pdf_links), " PDF links.")

# download each pdf link
walk(pdf_links, function(link) {
    fn <- basename(link)
    dest <- file.path(out_dir, fn)
    if (!file.exists(dest)) {
        message("Downloading ", fn, " …")
        resp <- try(GET(link, write_disk(dest, overwrite = TRUE)), silent = TRUE)
        if (inherits(resp, "try-error") || resp$status_code != 200) {
            warning("Failed to download: ", link, " (status: ", 
                    ifelse(inherits(resp, "try-error"), "error", resp$status_code), ")")
        }
    } else {
        message(fn, " already exists — skipping.")
    }
})

message("Download complete. Check directory: ", out_dir)
