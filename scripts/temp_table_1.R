library(pdftools)
library(tidyverse)

parse_tables <- function(url, remove_last = TRUE) {
  
  my_data <- pdf_data(url)
  
  lapply(my_data, function(my_data2) {
    header_row <- my_data2 %>%
      filter(y == min(y))
    
    subdata <- my_data2 %>%
      filter(y != min(y))
    
    first_table_temp <- subdata %>%
      group_by(y) %>%
      summarise(text = paste(text, space, collapse = " ")) %>%
      mutate(text = gsub("TRUE ", "", text),
             text = gsub("FALSE", ",,", text)) %>%
      mutate(text = lapply(strsplit(text, ",,"), str_trim)) %>%
      rowwise() %>%
      mutate(variable = list(tail(c("", header_row$text), length(text)))) %>%
      ungroup() %>%
      mutate(id = 1:nrow(.))
    
    if (remove_last) {
      first_table_temp <- first_table_temp %>%
        slice(-nrow(.))
    }
    
    first_table_temp %>%
      unnest() %>%
      spread(key = variable, value = text) %>%
      select(-y, -id) %>%
      select(one_of(c("V1", header_row$text))) %>%
      mutate_all(parse_guess)    
  })
}
