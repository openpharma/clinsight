## code to prepare metadata. Raw data is expected to be in Excel format.
library(here)

metadata <- get_metadata(filepath = here::here("data-raw/metadata.xlsx"))

usethis::use_data(metadata, overwrite = TRUE)