#' Get timeline data
#'
#' Function to create timeline data, to be used in a `timevis::timeline()`
#' object.
#'
#' @param data A list of data frames, with compatible clinical trial data.
#' @param table_data A list of data frames containing clinical trial data in
#'   wide format. Created with [create_table()].
#' @param timeline_cols Character vector with the name of the columns of the 
#'  output data frame.
#'
#' @return A data frame with timeline data.
#' @export
#' 
get_timeline_data <- function(
    data, 
    table_data, 
    timeline_cols =  c("subject_id", "event_name", "form_repeat", "item_group", 
                       "start", "group", "end", "title", "style", "id", "order")
    ){
  stopifnot(is.list(data), is.list(table_data))
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
      dplyr::mutate(group = "Visit")
  }
  
  if(is.null(table_data$`Adverse events`)){
    AE_timedata <- SAE_data <- data.frame()
  } else{
    AE_timedata <- table_data$`Adverse events` |> 
      dplyr::filter(!(`Serious Adverse Event` == "Yes" & 
                        .data[["start date"]] == .data[["SAE Start date"]])) |> 
      dplyr::mutate(
        event_name = `Name`,
        item_group = "Adverse events",
        group  = "Adverse event",
        `end date` = ifelse(`end date` == `start date`, NA_character_ , as.character(`end date`)) |> 
          as.character(),
        start = clean_dates(`start date`),
        end = clean_dates(`end date`),
        style = "background-color: #d47500;",
        title = `Name`
      )  
    
    SAE_data <- table_data$`Adverse events` |> 
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
        #end = clean_dates(ifelse(end == start, NA , end)),
        style = "background-color: #cd0200;",
        title = `Name`
      )
  } 
  
  drug_data <- if(is.null(data$General)){
    data.frame()
  } else{
    data$General |> 
      dplyr::filter(item_name %in% c("DrugAdminDate", "DrugDiscontDate")) |> 
      dplyr::mutate(
        event_name = gsub("DrugAdminDate", "Treatment", item_name),
        event_name = gsub(" date", "", event_name),
        group = "Events",
        start = clean_dates(item_value)
      )
  }
  
  df <- dplyr::bind_rows(study_event_data, AE_timedata, SAE_data, drug_data) |> 
    add_missing_columns(timeline_cols) |> 
    dplyr::mutate(
      id = dplyr::row_number(),
      group = factor(group, levels = c("SAE", "Adverse event", "Events", "Visit")),
      style = ifelse(!is.na(style), paste0(style, "line-height: 0.8; border-radius: 6px;"),
                     "line-height: 0.8; border-radius: 6px;"),
      order = as.numeric(group)
    ) |> 
    dplyr::filter(!is.na(subject_id)) |> 
    dplyr::select(dplyr::all_of(timeline_cols)) |> 
    dplyr::rename("content" = "event_name")
  df
}

#' Get available data  
#' 
#' Creates a data frame containing info about available data per individual, 
#' such as visits, adverse events, etc. Will be used in module [mod_queries_server()], 
#' to select available items to create a query for per individual and per form. 
#' Required columns are the ones distinctively identifying an item. 
#' For now that are site_code, event_name, subject_id, event_label, item_group, item_name.
#' 
#' @param data list of data frames to be used. Will be used for extracting the 
#' variables of interest from the study-specific forms.
#' @param tables list of tables to be used. Will be used for extracting the 
#' variables of interest from the common forms.
#' @param all_forms A data frame containing all forms. 
#' Mandatory columns are "form" (containing the form names), and "main_tab" 
#' (containing the tab name where the form should be located). 
#'
#' @return A data frame with available data points per form.
#' @export
#'
get_available_data <- function(data, tables, all_forms){
  stopifnot(is.list(data), is.list(tables))
  study_event_selectors <- lapply(
    all_forms$form, 
    \(x){
      if(with(all_forms, main_tab[form == x]) == "Study data"){
        if(is.null(data[[x]])) return(NULL)
        df_x <- data[[x]] |> 
          dplyr::select(
            dplyr::all_of(c("subject_id", "event_name", "event_label",  
                            "item_group", "item_name", "form_repeat"))
            )
      } else {
        if(is.null(tables[[x]])) return(NULL)
        df_x <- tables[[x]] |> 
          dplyr::select(subject_id, "item_name" = Name, form_repeat) |>
          dplyr::mutate(item_group = x, event_name = "Any visit", 
                        event_label = "Any visit") 
      }
      df_x |> 
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
        sprintf("%s (N: %s)", item_name, form_repeat), 
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
#' @param expected_general_columns Character vector with the expected columns. 
#' If columns are completely missing, they will be made explicitly missing in 
#' the data frame (that is, a column will be created with only missing character 
#' values). 
#'
#' @return A data frame with the overview data. Columns are: 
#' `subject_id`, `status`, `WHO.classification`, `Age`, `Sex`, `event_name`. 
#' @export
#'
get_static_overview_data <- function(
    data, 
    expected_general_columns = NULL
){
  stopifnot(is.list(data))
  expected_general_columns <- expected_general_columns %||% character(0)
  stopifnot(is.character(expected_general_columns))
  
  visits <- data |> 
    bind_rows_custom("item_value") |> 
    dplyr::filter(
      !is.na(event_name), 
      !event_name %in% c("Any visit", "Exit"),
      !is.na(subject_id)
    ) |> 
    dplyr::distinct(subject_id, event_name) |> 
    dplyr::arrange(subject_id, factor(event_name, levels = order_string(event_name))) |> 
    collapse_column_vals(group_by = "subject_id") |> 
    dplyr::distinct()
  
  create_table.general(
    data[["General"]], 
    expected_columns = expected_general_columns
    ) |>
    dplyr::select(tidyr::all_of("subject_id"), tidyr::any_of(c("status", "WHO.classification", "Age", "Sex"))) |>
    dplyr::left_join(visits, by = "subject_id")
}
