library(stringr)
library(tibble)

state <- "SA"
pages_to_ocr <- 54

award_path <- here::here("data-raw", "awards", paste0(state, ".pdf"))
raw_text <- pdftools::pdf_ocr_text(award_path, 
                                   pages = pages_to_ocr)
raw_lines <- raw_text %>%  
  str_split("\n") %>%
  unlist()

raw_ocr <- raw_lines %>% 
  str_replace_all("\\$ (\\d+)", "$\\1") %>%  # clean up '$ <digits>' 
  str_replace_all("(\\-\\d) \\$", "\\1 | $") %>% # add | after level names
  str_split_fixed("\\||(?<=\\d) [^\\$]* ?(?=\\$)", n = Inf) %>% # split
  as_tibble(.name_repair = "universal")

ocr_file <- here::here("data", paste0(state, "_ocr.xlsx"))
writexl::write_xlsx(raw_ocr, ocr_file)
