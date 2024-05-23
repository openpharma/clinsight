# This script creates small internal helper files that need to be available for 
# some functions in this package. 

# Color palette used in the shiny application for various figures.
col_palette <- c("#1a9641", "#a6d96a", "#fdae61", "#d7191c", "grey50")
names(col_palette) <- c("within limits", 
                        "out of limits, clinically insignificant", 
                        "out of limits, significance unknown", 
                        "out of limits, clinically significant", 
                        "limits unknown")

# Empty data frame used to reset query data frame if needed.
query_data_skeleton <- dplyr::tibble(
  "query_id"      = character(),
  "major"         = character(),
  "subject_id"    = character(),
  "event_label"   = character(),
  "item_group"    = character(),
  "item"          = character(),
  "timestamp"     = character(),
  "n"             = numeric(),     
  "reviewer"      = character(),
  "query"         = character(),
  "resolved"      = character(),
  "resolved_date" = character(),
  "edit_reason"   = character()
)

# columns names that are required for the application to function. 
# Used to test whether these are all available in the column_specs when reading 
# in data with get_raw_data().
required_col_names <- c(
  "site_code", 
  "subject_id", 
  "event_id", 
  "event_date", 
  "event_name", 
  "event_repeat", 
  "form_id", 
  "form_repeat", 
  "var", 
  "item_value", 
  "edit_date_time"
  )


usethis::use_data(col_palette, query_data_skeleton, required_col_names,
                  overwrite = TRUE, internal = TRUE)
