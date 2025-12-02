#' Get timeline data
#'
#' Function to create timeline data, to be used in a `timevis::timeline()`
#' object.
#'
#' @param data A list of data frames, with compatible clinical trial data.
#' @param table_data A list of data frames containing clinical trial data in
#'   wide format. Created with [create_table()].
#' @param timeline_cols Character vector with the name of the columns of the
#'   output data frame.
#' @param treatment_label Character vector with the label to use for the
#'   treatment item in the timeline.
#'
#' @return A data frame with timeline data.
#' @keywords internal
#' 
get_timeline_data <- function(
    data, 
    table_data, 
    timeline_cols =  c("subject_id", "event_name", "form_repeat", "item_group", 
                       "start", "group", "end", "title", "className", "id", "order"),
    treatment_label = "\U1F48A T\U2093"
){
  stopifnot(is.list(data), is.data.frame(table_data))
  stopifnot(is.character(timeline_cols), is.character(treatment_label))
  
  if(all(unlist(lapply(data, is.null)))) return({
    warning("No data found. Returning empty data frame")
    setNames(
      as.data.frame(matrix(ncol = length(timeline_cols))),
      timeline_cols
    ) |>
      dplyr::rename("content" = "event_name")
  })
  study_event_data <- if(is.null(data) ){
    data.frame()
  } else{
    data |> 
      bind_rows_custom("item_value") |> 
      dplyr::filter(
        !is.na(event_name), 
        !is.na(event_date),
        event_name != "Any visit"
      ) |> 
      dplyr::distinct(subject_id, event_name, start = event_date) |> 
      dplyr::mutate(
        group = "Visit",
        title = paste0(start, " | ", event_name)
      )
  }
  
  if(nrow(table_data) == 0){
    AE_timedata <- SAE_data <- data.frame()
  } else{
    AE_timedata <- table_data |> 
      dplyr::filter(!(`Serious Adverse Event` == "Yes" & 
                        .data[["start date"]] == .data[["SAE Start date"]])) |> 
      dplyr::mutate(
        event_name = `Name`,
        item_group = "Adverse events",
        group  = "Adverse event",
        `end date` = ifelse(
          `end date` == `start date`, 
          NA_character_ , 
          as.character(`end date`)
        ) |> 
          as.character(),
        start = clean_dates(`start date`),
        end = clean_dates(`end date`),
        className = "bg-warning",
        title = paste0(
          start, ifelse(!is.na(end), paste0(" - ", end), ""), 
          " | ", `Name`
        )
      )  
    
    SAE_data <- table_data |> 
      dplyr::filter(`Serious Adverse Event` == "Yes") |> 
      dplyr::mutate(
        event_name = `Name`,
        item_group = "Adverse events",
        `end date` = as.character(`end date`),
        group  = "SAE",
        `SAE End date` = dplyr::case_when(
          all(is.na(`SAE End date`) ) ~ NA_character_, 
          `SAE End date` == `SAE Start date` ~ NA_character_ , 
          TRUE ~ as.character(`SAE End date`)
        ),
        start = ifelse(is.na(`SAE Start date`), clean_dates(`start date`), 
                       clean_dates(`SAE Start date`)) |> 
          as.Date(),
        end = clean_dates(`SAE End date`),
        className = "bg-danger",
        title = paste0(
          start, 
          ifelse(!is.na(end), paste0(" - ", end), ""), 
          " | ", 
          `Name`
        )
      )
  } 
  
  drug_data <- if(is.null(data$General)){
    data.frame()
  } else{
    df_drug_admin <- data$General[
      data$General$item_name %in% c("DrugAdminDate", "DrugAdminDose"), 
    ] |> 
      tidyr::pivot_wider(names_from = item_name, values_from = item_value) |> 
      add_missing_columns(c("DrugAdminDate", "DrugAdminDose")) |> 
      dplyr::mutate(
        event_name = treatment_label,
        group = "Events",
        start = clean_dates(DrugAdminDate),
        title = paste0(
          DrugAdminDate, " | ",
          "Treatment \n", 
          "Dose: ", ifelse(is.na(DrugAdminDose), "?", DrugAdminDose)
        )
      ) |> 
      dplyr::select(-dplyr::all_of(c("DrugAdminDate", "DrugAdminDose")))
    df_discont <- data$General[
      data$General$item_name %in% c("DrugDiscontDate"), 
    ] |> 
      dplyr::mutate(
        event_name = "Drug discontinuation",
        group = "Events",
        start = clean_dates(item_value),
        title = paste0(start, " | ", "Treatment discontinued")
      )
    dplyr::bind_rows(df_drug_admin, df_discont)
  }
  
  df <- dplyr::bind_rows(study_event_data, AE_timedata, SAE_data, drug_data) |> 
    add_missing_columns(timeline_cols) |> 
    dplyr::mutate(
      id = dplyr::row_number(),
      className = ifelse(is.na(className), "bg-light", className),
      group = factor(group, levels = c("SAE", "Adverse event", "Events", "Visit")),
      order = as.numeric(group)
    ) |> 
    dplyr::filter(!is.na(subject_id), !is.na(start)) |> 
    dplyr::select(dplyr::all_of(timeline_cols)) |> 
    dplyr::rename("content" = "event_name")
  df
}

#' Get available data
#'
#' Creates a data frame containing info about available data per individual,
#' such as visits, adverse events, etc. Will be used in module
#' [mod_queries_server()], to select available items to create a query for per
#' individual and per form. Required columns are the ones distinctively
#' identifying an item. For now that are site_code, event_name, subject_id,
#' event_label, item_group, item_name.
#'
#' @param data list of data frames to be used. Will be used for extracting the
#'   variables of interest from the study-specific forms.
#' @param form_repeat_name A character string with the name of the `form_repeat`
#'   variable. This variable (with this name) will be added to the item name if
#'   duplicate names exist for each participant.
#'
#' @return A data frame with available data points per form.
#' @keywords internal
#' 
get_available_data <- function(
    data, 
    form_repeat_name = "N"
){
  stopifnot(is.list(data), is.character(form_repeat_name))
  if(identical(form_repeat_name, character(0))){form_repeat_name <- "N"}
  selector_cols <- c("subject_id", "item_name", "form_repeat", "item_group", 
                     "event_name", "event_label")
  if(length(data) == 0) {
    warning("Empty list of data provided")
    return(add_missing_columns(data.frame(), c(selector_cols, "n")))
  }
  study_event_selectors <- lapply(
    data, 
    \(x){
      name_vars <- c("Name", "AE Name", "CP Name", "MH Name", "CM Name")
      if ( any(unique(x$item_name) %in% name_vars)){
        x <- x[x$item_name %in% name_vars, ] |> 
          dplyr::mutate(item_name = item_value)
      }
      x[c(selector_cols)] |> 
        dplyr::distinct() |> 
        dplyr::arrange(
          subject_id, 
          factor(event_name, levels = order_string(event_name))
        )
    }) |> 
    dplyr::bind_rows()
  # To uniquely identify events with the same name (mostly in common_forms):
  study_event_selectors |> 
    dplyr::mutate(
      n = dplyr::n(), 
      .by = c("subject_id", "item_group", "event_name", "item_name")
    ) |> 
    dplyr::mutate(
      item_name = ifelse(
        n > 1, 
        sprintf("%s (%s: %s)", item_name, form_repeat_name, form_repeat), 
        item_name
      )
    ) |> 
    dplyr::select(-n)
}


#' Create static overview data
#'
#' Creates overview data of each patient in the study. Used to create the start
#' page of the application.
#'
#' @param data List of data frames.
#' @param available_data A data frame with available data. Visits will be
#'   extracted from here. Required columns are `subject_id`, `event_name`,
#'   `event_label`. The `event_label` variable should be a factor in order to
#'   work well with the function [fig_timeline()].
#' @param expected_general_columns Character vector with the expected columns.
#'   If columns are completely missing, they will be made explicitly missing in
#'   the data frame (that is, a column will be created with only missing
#'   character values).
#'
#' @return A data frame with the overview data. Columns are: `subject_id`,
#'   `status`, `WHO.classification`, `Age`, `Sex`, `event_name`.
#'
#' @keywords internal
#' 
get_static_overview_data <- function(
    data, 
    available_data,
    expected_general_columns = NULL
){
  stopifnot(is.list(data))
  expected_general_columns <- expected_general_columns %||% character(0)
  stopifnot(is.character(expected_general_columns))
  visits <- with(available_data, available_data[
    !is.na(event_name) & !event_name %in% c("Any visit", "Exit") &!is.na(subject_id),
  ]) |> 
    dplyr::arrange(subject_id, event_label) |> 
    dplyr::distinct(subject_id, event_name) |> 
    collapse_column_vals(group_by = "subject_id") |> 
    dplyr::distinct()
  
  create_table.general(
    data[["General"]], 
    expected_columns = expected_general_columns
  ) |>
    dplyr::left_join(visits, by = "subject_id")
}
