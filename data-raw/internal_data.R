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
  "type"         = character(),
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

# columns specifications required for the application to function properly. 
# Used to convert columns to the right format, and to test whether all the required 
# columns are available when merging raw data with metadata.
clinsight_col_specs <- c(
  "site_code" = "c", 
  "subject_id" = "c", 
  "event_id" = "c", 
  "event_date" = "D", 
  "event_name" = "c", 
  "event_repeat" = "i", 
  "form_id" = "c", 
  "form_repeat" = "i", 
  "var" = "c", 
  "item_value" = "c", 
  "edit_date_time" = "T",
  ".default" = "c"
  ) |> 
  readr::as.col_spec()

# Required columns needed in study data for the application to function.
required_col_names <- names(clinsight_col_specs$cols)


usethis::use_data(col_palette, query_data_skeleton, required_col_names, 
                  clinsight_col_specs, overwrite = TRUE, internal = TRUE)
