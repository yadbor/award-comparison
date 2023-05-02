library(rvest)
library(tibble)
library(dplyr)

# Set to TRUE for messages
verbose = TRUE

# Keep the awards here
award_folder <- here::here("data-raw", "awards")
if (!dir.exists(award_folder)) { 
  if (verbose) {
    cat("creating 'awards' folder.\n")
  }
  dir.create(award_folder)
}

# Extract the names of the files for the current Award in each State
# and download them for later analysis.

# If using Windows can get the current proxy info
# Set the https proxy from the Windows environment (using httr and curl).
config_proxy <- httr::use_proxy(
  url = curl::ie_get_proxy_for_url("https://www.google.com.au"), 
  auth = "ntlm", 
  username = ""
)

# Get the link to the salary document from the appropriate State website.
# This is for websites which have a list of links to files for the Awards etc.
# Identified as type == "IDX" in the sources table.
# The order argument determines whether the links are earliest first (fwd)
# or latest first (rev). Only the latest link is returned, determined by order.
doc_from_index <- function(url, pattern = "", order = "fwd",
                           config = config_proxy) {
  # Check option
  order <- tolower(order)
  if (!(order %in% c("fwd", "rev"))) {
    stop("order must be 'fwd' or 'rev'")
  }
  # str_subset chokes on "", despite what documentation says
  # and also on non-character NA, so fix both here
  if(pattern == "" | is.na(pattern)) {
    pattern <- NA_character_
  }

  cat("dir ", order, " pat ", pattern, " from ", url, "\n")
  
  doc_list <- httr::GET(url, config = config_proxy) %>%
    rvest::read_html() %>% 
    rvest::html_elements("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset(pattern = pattern) %>%
    rvest::url_absolute(base = url)
  
  doc_url <- ifelse(order == "fwd", 
                      dplyr::last(doc_list), 
                      dplyr::first(doc_list)
                    )
  cat(order, "\n", 
      doc_url, "\n\t",
        dplyr::last(doc_list), "\n\t", 
        dplyr::first(doc_list), "\n\n")
  
  return(doc_url)
}

# Where does each State keep their Award/Salary table?
# Most are lists of PDFs (type = IDX), but Qld is directly in the page.
# Extract the links in each list and match to the supplied pattern.
# There can be multiple hits, so try to choose the latest entry
# - either the last if in 'fwd' order, or the first if in 'rev' order.
# If the file is a scanned PDF need OCR to extract text, so note file type.
#
sources <- readr::read_csv(here::here("data", "sources.csv"), 
                           show_col_types = FALSE)

sources <- sources %>% 
  mutate(award_file = award_file(state, doc_type, award_folder))

if (verbose) {
  cat("Fetching the URLs of the most recent award files...\n")
}

# Find the appropriate documents containing the salary scales
sources <- sources %>% 
  rowwise() %>% # need this as httr::GET in doc_from_index is not vectorised
  mutate(
    doc_url = if_else(type == "IDX", doc_from_index(url, pattern, order), url)
  )

if (verbose) {
  cat("done.\n")
}

award_file <- function(state, doc_type, dest_dir) {
  here::here(dest_dir, paste(toupper(state), tolower(doc_type), sep = "."))
}

# Add the download path to sources
sources <- sources %>%
  mutate(award_file = award_file(state, doc_type, award_folder))

# Save an Award file for later processing.
download_award <- function(doc_url, state, doc_type, ..., dest_dir) {
  dest <- award_file(state, doc_type, dest_dir)
  if (verbose) {
    cat(doc_url, "\nto\t", dest, "\n")
  }
  download.file(url = doc_url, destfile = dest, mode = "wb")
}

# Wrap with safely() to allow pwalk to continue even if the download fails
download_safely <- purrr::safely(download_award)


if (verbose) {
  cat("Downloading award files...\n\n")
}
# Apply to all rows in sources
purrr::pwalk(sources, download_safely, dest_dir=award_folder)

if (verbose) {
  cat("done.\n")
}