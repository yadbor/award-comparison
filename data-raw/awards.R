## code to prepare `awards` dataset goes here

usethis::use_data(awards, overwrite = TRUE)

sources <- readr::read_csv(here::here("data", "sources.csv"))


get_from_html <- function(filename, order) {
  if (order == "fwd") {
    rvest::read_html(filename) %>% 
      html_table() %>%
      dplyr::last()
  } else if (order == "rev") {
    rvest::read_html(filename) %>% 
      html_table() %>%
      dplyr::first()
  } else {
    simpleError("order must be either 'fwd' or 'rev'")
  }
}

table_structures <- tibble::tribble(
  ~state, ~pattern, ~skip,
  "QLD", "", 0,
  "WA", "SALARIES – PROFESSIONAL DIVISION", 3,
  "ACT", "Health Professional Level 1", 0,
  "NSW", "Biomedcial Engineer", 0
)

# Make a list to hold all the salary tables
salaries <- list()

# for OLD - HTML
this_state <- "QLD"
current <- sources %>% filter(state == this_state)
struct <- table_structures %>% filter(state == current$state)
current_salaries <- get_from_html(current$award_file, current$order)
salaries[[this_state]] <- current_salaries

# for WA - PDF
# fixed columns, skip first 3 rows, 
# only keep columns with headers (so are character)
this_state <- "WA"
current <- sources %>% filter(state == this_state)
struct <- table_structures %>% filter(state == current$state)

pages <- pdftools::pdf_info(current$award_file)$pages
raw_text <- pdftools::pdf_text(current$award_file)
pages_read <- length(raw_text)
first_to_read <- 1

has_text <- any(nchar(raw_text) > 15) # does any page have > 15 characters?

if (is.list(raw_text) & pages_read == pages) {
  # It's a text PDF, so just read the text
  # Find the page with our pattern, then split into lines
  raw_text <- raw_text %>%
    stringr::str_subset(struct$pattern) %>% 
    stringr::str_split("\n") %>% 
    unlist()
} else {
  # try by ocr
  raw_text <- pdftools::pdf_ocr_text(current$award_file, 
                                     pages = 1:pages)
}

# Skip unwanted rows above the column headers
raw_text <- tail(raw_text, length(raw_text) - struct$skip)

# Extract the table
salaries[[this_state]] <- readr::read_fwf(I(raw_text)) %>% 
  select(where(is.character))


table_start_pattern <- "Table 1 - Salaries and Allowances"
table_end_pattern <- "PART C"

skip_pattern <- "(^\\s+- \\d+ -$)"


# NSW - crosses page boundary
# For potentially multi-page tables need to combine the pages first
raw_lines <- raw_text %>% stringr::str_split("\n") %>% unlist()
# find the lines containing the table of interest
# Choose the last found in case it's also a heading in the table of contents
table_start <- raw_lines %>% 
  stringr::str_which(table_start_pattern) %>% 
  tail(1)
table_end <- raw_lines %>% 
  stringr::str_which(table_end_pattern) %>% 
  tail(1)
# Just get the table part, skipping any footers etc. defined by skip_pattern
table_lines <- raw_lines[seq(table_start+1, table_end-1)] %>% 
  stringr::str_subset(pattern = skip_pattern, negate = TRUE)

# Column separators are 2 or more spaces
salaries <- table_lines %>% 
  stringr::str_replace_all("\\s{2,}", "|") %>% 
  stringr::str_split("\\|", simplify=TRUE) %>% 
  tibble::enframe()

# another option
# Column separators are 2 or more spaces
salaries <- table_lines %>% 
  stringr::str_replace_all("\\s{2,}", "|") %>% 
  tibble::enframe() %>% 
  tidyr::separate(value, 
                  into = c("class", "frequency", "pay1", "pay2"), 
                  sep = "\\|")
# That works for the data rows, but can't get the dates for the columns
# Get them from the header bits maybe?

# Yet another option
# Get the column positions from the first few lines & use for the whole table
col_spec <- readr::fwf_empty(I(table_lines[1:20]))
salaries <- readr::read_fwf(I(table_lines), col_positions = col_spec)




table_layout <- tibble::tribble(
~state, ~start_pattern, ~start_offset, ~end_pattern, ~end_offset, ~skip_pattern,
"WA",   "SALARIES – PROFESSIONAL DIVISION", 2, 
        "SCHEDULE\\s+4", -2, 
        "^\\s+\\d+$",
"NSW",  "Table 1 - Salaries and Allowances", 2,
        "PART C", -2,
        "(^\\s+- \\d+ -$)"
)

extract_table <- function(lines, table_def) {
  table_start <- lines %>%
    stringr::str_which(table_def$start_pattern) %>%
    tail(1) + table_def$start_offset
  table_end <- lines %>%
    stringr::str_which(table_def$end_pattern) %>%
    tail(1) + table_def$end_offset
  
  raw_lines[seq(table_start, table_end)] %>%
    stringr::str_subset(pattern = table_def$skip_pattern, negate = TRUE)
}

