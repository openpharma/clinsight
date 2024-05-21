#' Fill Same columns
#'
#' Goal of the function is to 1) expand a missing variable so that it is
#' available everywhere, and 2) fill missing values in columns that are the same
#' for each group within a long-form data frame in every newly created row.
#' Experimental function. Do not (yet) use in production.
#'
#' @param data A data frame with data (raw data merged with metadata).
#'
#' @return A data frame
#' 
fill_same_cols <- function(
    data = merged_data,
    join_cols = c("site_code", "subject_id", "event_id", "event_date", 
                  "event_name", "event_repeat", "form_id", "form_repeat", 
                  "region", "day", "event_label", "item_type"),
    group = "Medication",
    fill_item = "CM MDS/AML Specific"
){
  med_data <- data |> 
    dplyr::filter(item_group == group)
  data |> 
    dplyr::filter(item_group == group) |> 
    dplyr::select(dplyr::all_of(c(join_cols, "item_group"))) |> 
    dplyr::distinct() |> 
    dplyr::mutate(item_name = fill_item) |> 
    dplyr::full_join(data, by = c(join_cols, "item_group", "item_name")) 
  
  # below works, but is very slow, and not tested on all edge-cases 
  # (does it always expand the correct column? what if there is only one data 
  # point available and the rest is missing, should every row really get the 
  # value of that datapoint?
  # Probably not --> probably better would be to manually define the columns that need 
  # to be filled. It would certainly be faster. 
  # med_data3 <- split(med_data2, ~subject_id+event_repeat)
  # lapply(
  #   med_data3, 
  #   \(x){
  #     lapply(names(x), \(column){
  #       if(column %in% join_cols || all(is.na(x[[column]])) )  return(x[column])
  #       n_vals <- unique(na.omit(x[[column]]))
  #       if(length(n_vals) == 1) x[[column]] <- n_vals
  #       x[column]
  #     }) |> 
  #       dplyr::bind_cols()
  #   }) |> 
  #   bind_rows_custom("day")
}
