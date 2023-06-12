## code to prepare `awards` dataset goes here

library(dplyr)
library(stringr)

##usethis::use_data(awards, overwrite = TRUE)

source(here::here("scripts", "folder_locations.R"))

# Read the list of source info and the names of the downloaded files.
# Should check for error if this is missing, but the job will fail anyway.
sources <- readr::read_csv(here::here("data", "sources_and_file.csv"), 
                           show_col_types = FALSE)

# Having downloaded the relevant current Award files, extract the salary tables.
# First extract each Award to a separate files, then combine them
# into one data frame.


# For the two scanned/OCR files the tables are manually copied into .xlsx files
# as they need a lot of clean-up.

# If the data are already in a .html file, then extract the relevant table.

# If the data are in a PDF file use some info about the structure of the tables
# to extract the salary information.


table_from_html <- function(filename, order) {
  
  tables <- rvest::read_html(filename) %>% rvest::html_table()
    
  if (order == "fwd") {
      dplyr::last(tables)
  } else if (order == "rev") {
      dplyr::first(tables)
  } else {
    simpleError("order must be either 'fwd' or 'rev'")
  }
}

table_rows_from_text_1 <- function(raw_text, tbl_layout, skip_blank_rows = TRUE) {
  # For potentially multi-page tables need to combine the pages first
  raw_lines <- raw_text %>% stringr::str_split("\n") %>% unlist()
  # find the lines containing the table of interest
  # Choose the last found in case it's also a heading in the table of contents
  table_start <- raw_lines %>% 
    stringr::str_which(tbl_layout$start_pattern) %>% 
    tail(1) + tbl_layout$start_offset
  table_end <- raw_lines %>% 
    stringr::str_which(tbl_layout$end_pattern) %>% 
    tail(1) + tbl_layout$end_offset
  # Just get the table part, skipping any footers etc. defined by skip_pattern
  table_lines <- raw_lines[seq(table_start, table_end)] %>% 
    stringr::str_subset(pattern = tbl_layout$skip_pattern, negate = TRUE)
  if (skip_blank_rows) {
    table_lines <- table_lines %>% 
      str_trim() %>% 
      str_subset("^\\s*$", negate = TRUE)
  }
  return(table_lines)
}

table_rows_from_text <- function(raw_text, award_def, skip_blank_rows = TRUE) {
  
  # Split into lines, combining pages to allow for multi-page tables
  raw_lines <- raw_text %>% stringr::str_split("\n") %>% unlist()
  
  # Find the lines containing the table of interest, defined by patterns
  # Choose the last found in case it's also a heading in the table of contents
  # If no pattern is given use the first or last row as appropriate
  if (award_def$start_pattern == "") {
    table_start <- 1 # the first line
  } else {
    table_start <- raw_lines %>% 
      stringr::str_which(award_def$start_pattern) %>% 
      tail(1)
  }
  table_start <- table_start + award_def$start_offset
  if (award_def$end_pattern == "") {
    table_end <- length(raw_lines) # the last line
  } else {
    table_end <- raw_lines %>% 
    stringr::str_which(award_def$end_pattern) %>% 
    tail(1)
  } 
  table_end <- table_end + award_def$end_offset
  
  # Just the table part
  table_lines <- raw_lines[seq(table_start, table_end)] 
  
  # Skip any footers etc. defined by skip_pattern
  if(award_def$skip_pattern != "") {
    table_lines <- stringr::str_subset(table_lines, 
                                       pattern = award_def$skip_pattern, 
                                       negate = TRUE)
  }
  
  # Optionally remove any empty rows
  if (skip_blank_rows) {
    table_lines <- table_lines %>% 
      str_subset("^\\s*$", negate = TRUE)
  }
  return(table_lines)
}

save_raw_table <- function(raw_table, s) {
  #readr::write_csv(raw_table, here::here(raw_table_folder, paste0(s, ".csv")))
  cat(here::here(raw_table_folder, paste0(s, ".csv")))
}


table_layout <- tibble::tribble(
  ~state, 
  ~start_pattern, ~start_offset, ~end_pattern, ~end_offset, ~skip_pattern,
  
  "QLD", # Source is html so table is already properly defined
  "", 0, 
  "", 0, 
  "",
  
  "WA", 
  "SALARIES – PROFESSIONAL DIVISION", 2, 
  "SCHEDULE 3", -2, 
  "^\\s+\\d+$",
  
  "NSW", 
  "PART B", 2, 
  "PART C", -2, 
  "(^\\s+- \\d+ -$)",
  
  "ACT", 
  "ANNEX A – CLASSIFICATIONS AND RATES OF PAY", 1, 
  "ANNEX B", -1, 
  "Page\\s+\\d{1,3}\\s+of\\s+\\d{1,3}",
  
  "VIC",
  "Classification\\s+FFPPOA", 0, 
  "Allowances", -2, 
  "",
  
  "SA", # Scanned & need to be extracted manually
  NA, NA, NA, NA, NA, 
  "TAS", # Scanned & need to be extracted manually
  NA, NA, NA, NA, NA, 
)

MRWA_layout <- tibble::tribble(
  ~state, 
  ~start_pattern, ~start_offset, ~end_pattern, ~end_offset, ~skip_pattern,
  "MRWA",
  "ii\\)\\s+Specified Calling", 1,
  "ii\\)\\s+Specified Callings cont\\.", 0,
  "^\\s*\\d{1,3}\\s*$"
)  

# Combine all information about each State Award in one table
award_info <- dplyr::left_join(sources, table_layout, by = "state")

# Extract the rough salary tables.
# These may still need human cleaning before they are useful. 
# In particular, anything that needs OCR will need a lot of manual cleaning.
salary_table <- list()
for (s in pull(award_info, state)) {
  current <- award_info %>% filter(state == s)   # All info for the state
  
  cat("process ", s, "\t type ", current$doc_type, "\n")
  
  if (current$doc_type == "html") {
    raw_table <- table_from_html(current$award_file, current$order)
  } else if (current$doc_type == "pdf") {
    raw_text <- pdftools::pdf_text(current$award_file) # Text in the PDF
    raw_table <- table_rows_from_text(raw_text, current)
  } else if (current$doc_type == "scan.pdf") {
    cat(current$award_file, " is a scanned PDF - need to copy data manually.\n")
    next
  } else {
    cat(s, ": can't interpret files of type ", current$doc_type, ".\n")
    next
  }
  # Save the extracted but still messy salary table
#  readr::write_csv(raw_table, here::here(raw_table_folder, paste0(s, ".csv")))
  save_raw_table(raw_table, s)
  salary_table[[s]] <- raw_table
}


current <- award_info %>% filter(state == this_state) # All info for the state
raw_text <- pdftools::pdf_text(current$award_file)

tbl_layout <- filter(table_layout, state == this_state)

table_rows_from_text(raw_text, tbl_layout)




# Make a list to hold all the salary tables
salaries <- list()

# for OLD - HTML
this_state <- "QLD"
current <- sources %>% filter(state == this_state)
#struct <- table_structures %>% filter(state == current$state)
current_salaries <- table_from_html(current$award_file, current$order)
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
    stringr::str_split("\n") %>% 
    unlist() %>%
    stringr::str_subset(struct$pattern)
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

# trivial change to force commit

make_columns_spec <- function(a_table) {
  if (is.character(a_table)) {
    # a string, so try to split into columns
    # Get the column positions from the first few lines & use for the whole table
    few_start <- min(10, length(a_table)) # Might be < 10 lines
    few_end  <- min(20, length(a_table)) # Might be < 20 lines
    col_spec <- readr::fwf_empty(I(a_table[few_start:few_end]))
    a_table <- readr::read_fwf(I(a_table), col_positions = col_spec)
  } 
  return(a_table)
}

make_columns_rep <- function(a_table) {
  if (is.character(a_table)) {
    # a string, so try to split into columns
    # Get the column positions from the first few lines & use for the whole table
    a_table %>% 
      stringr::str_replace_all("\\s{2,}", "|") %>% 
      tibble::as_tibble_col() %>% 
      tidyr::separate(value, 
                      into = c("class", "frequency", "pay1", "pay2", "more"),
                      extra = "merge",
                      sep = "\\|")
  } else {
    return(a_table)
  } 
}

# Manually extract


QLD <- salary_table$QLD # Already a tibble, just fix up the column names
# Names are in first row, but can't use slice() as have duplicate column names
names(QLD) <- as.character(QLD[1, ])
QLD <- slice(QLD, -1) # drop the first row
VIC <- readr::read_fwf(I(salary_table$VIC[1:21])) # extra headers to trim
WA <- readr::read_fwf(I(salary_table$WA)) # extra headers to trim

nums <- str_which(salary_table$ACT, "CLASSIFICATION")[-1]
tmp <- salary_table$ACT[-nums]
ACT <- tmp %>% 
  str_replace_all("\\s{2,}", "\\|") %>% 
  str_split("\\|", simplify = TRUE) %>% 
  as_tibble(.name_repair = "unique")

tmp <-  str_subset(salary_table$NSW, "Per Week\\s+\\d", negate = TRUE)
NSW <- tmp %>% 
  str_replace_all("\\s{2,}", "\\|") %>% 
  str_split("\\|", simplify = TRUE) %>% 
  as_tibble(.name_repair = "unique")

#col_pos <- readr::fwf_empty(I(tmp), 10, n = 10)
#ACT <- readr::read_fwf(I(tmp), col_positions = col_pos)

# SA & TAS done via OCR and manual intervention
SA = readxl::read_xlsx(here::here("data", "SA_manual.xlsx"))
TAS = readxl::read_xlsx(here::here("data", "TAS_manual.xlsx"))


sheet_list <- list(ACT=ACT, NSW=NSW, QLD=QLD, SA=SA, TAS=TAS, VIC=VIC, WA=WA)

writexl::write_xlsx(sheet_list,
                    path = here::here("data", "awards_dirty.xlsx"),
                    col_names = TRUE,
                    format_headers = FALSE)
