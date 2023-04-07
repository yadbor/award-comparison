library(rvest)
library(tibble)
library(dplyr)

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
  pattern <- ifelse(pattern == "", NA_character_, pattern) 
  
  doc_list <- httr::GET(url, config = config_proxy) %>%
    rvest::read_html() %>% 
    rvest::html_elements("a") %>% 
    rvest::html_attr("href") %>% 
    stringr::str_subset(pattern = pattern) %>%
    rvest::url_absolute(base = url)
  
  return(ifelse(order == "fwd", 
                dplyr::last(doc_list), 
                dplyr::first(doc_list))
  )
}

# Call doc_from_index on a data.frame or tibble (or indeed data.table)
helper <- function(df){
  doc_from_index(url = df$url, 
                 pattern = df$pattern, 
                 order = df$order)
}


# Where does each State keep their Award/Salary table?
# Most are lists of PDFs (type = IDX), but Qld is directly in the page.
# Extract the links in each list and match to the supplied pattern.
# There can be multiple hits, so try to choose the latest entry
# - either the last if in 'fwd' order, or the first if in 'rev' order.
# If the file is a scanned PDF need OCR to extract text, so note file type.
# Probably move this out to an external file when the code is stable.
sources <- tibble::tribble(
  ~state, ~url, ~type, ~pattern, ~order, ~doc_type,
  
  "ACT",
  "https://www.canberrahealthservices.act.gov.au/careers/enterprise-agreeements",
  "IDX",
  "Health.*Professional.*Enterprise.*Agreement",
  "fwd",
  "PDF",
  
  "QLD", 
  "https://www.health.qld.gov.au/hrpolicies/salary/health-practitioners", 
  "WEB", 
  "",
  "",
  "",
  
  "NSW", 
  "https://www.health.nsw.gov.au/careers/conditions/Pages/b.aspx",
  "IDX",
  "profmed",
  "fwd",
  "PDF",
  
  "SA",
  "https://www.agd.sa.gov.au/industrial-relations/current-agreements",
  "IDX",
  "Public.*Sector.*Salaried",
  "fwd",
  "scan",
  
  "TAS",
  "https://www.tic.tas.gov.au/public_sector_agreements",
  "IDX",
  "Allied.*Health.*Professionals",
  "rev",
  "scan",
  
  "VIC",
  "https://www.westernhealth.org.au/Careers/Pages/Salary-Rate.aspx",
  "IDX",
  "Biomed",
  "fwd",
  "PDF",
  
  "WA",
  "https://www.health.wa.gov.au/articles/a_e/awards-and-agreements",
  "IDX",
  "Salaried",
  "fwd",
  "PDF"
)

# Find the appropriate documents containing the salary scales
sources <- sources %>% 
  rowwise() %>% # need this as httr::GET in doc_from_index is not vectorised
  mutate(
    doc = if_else(
      type == "IDX", 
      doc_from_index(url, pattern), 
      url
    )
  )

# Save the Award files for later processing.


