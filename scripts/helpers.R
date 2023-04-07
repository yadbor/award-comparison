make_raw_dir <- function(dirname) {
  if (!dir.exists(here::here("data-raw", dirname))) { 
    dir.create(here::here("data-raw", dirname))
  } else {
    simpleWarning(paste0("folder '", dirname, "' already exits.\n"))
  }
}
