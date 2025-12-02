#' Count Adverse Events
#'
#' Simple helper function to count Adverse Events (AEs) and Serious Adverse
#' Events (SAEs).
#'
#' @param data A data frame with Adverse Event data. Required columns are the
#'   clinsight `key_cols` and the column `item_value`.
#'
#' @returns A data frame with the columns `subject_id`, `AEs` (number of AEs per
#'   subject), `SAEs` (number of SAEs per subject).
#' @keywords internal
count_adverse_events <- function(
    data,
    all_ids = NULL,
    SAE_column_name = "Serious Adverse Event"
    ){
  stopifnot(is.data.frame(data))
  if (nrow(data) == 0 ) {
    return({
      data.frame(subject = character(), AEs = numeric(), SAEs = numeric())
    })
  }
  stopifnot("One or more required columns are missing" = all(c(key_columns, "item_value") %in% names(data)))
  stopifnot(is.character(all_ids %||% ""))
  all_ids <- c(all_ids, unique(data[["subject_id"]]))
  if (!SAE_column_name %in% data$item_name) {
    warning("item '", SAE_column_name, "' not found. Unable to determine (S)AE numbers.")
    return(
      data.frame(subject_id = all_ids, AEs = "?", SAEs = "?")
    )
  }
  
  ae_data <- dplyr::left_join(
    unique(data[c("subject_id", "form_repeat")]),
    unique(data[data$item_name %in% SAE_column_name, c(key_columns, "item_value")]),
    by = c("subject_id", "form_repeat")
  ) |> 
    dplyr::mutate(
      item_value = ifelse(is.na(item_value), "No", item_value)
    )
  all_aes <- data.frame(subject_id = unique(all_ids)) |> 
    dplyr::left_join(
      ae_data, 
      by = "subject_id"
    )
  
  all_aes |> 
    dplyr::summarize(
      AEs =  sum(item_value == "No", na.rm = TRUE),
      SAEs = sum(item_value == "Yes", na.rm = TRUE),
      .by = subject_id
    )
}
