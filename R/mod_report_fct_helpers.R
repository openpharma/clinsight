
#' Select report data
#' 
#' Helper function for mod_report. Filters the correct report data and renames 
#' the columns to a more report-friendly name.
#'
#' @param data A data frame with the report data.
#' @param report_table Character vector. Can be either "review_data" or "query_data".  
#' @param report_type Character vector. Controls which data will be included in the report. 
#' Either "session" (only data from the current session and review will be included), 
#' or "all" (all review data will be included). 
#' @param report_reviewer Character vector with the name of the current reviewer. 
#' Used to filter the data. Will be ignored if report_type is not "session".
#' @param include_from_date Character vector with the date from which data 
#' should be included. Will be ignored if report_type is not "session".
#'
#' @return A data frame with the required report data.
#' @export
#'
select_report_data <- function(
    data, 
    report_table, 
    report_type, 
    report_reviewer,
    include_from_date = input$include_from_date
){
  stopifnot(is.data.frame(data))
  
  df <- data
  if(report_table == "review_data") return({
    if(report_type == "session"){
      df <- df |> 
        with(df[!is.na(reviewer) & reviewer == report_reviewer & 
                  timestamp >= include_from_date & reviewed == "Yes", ])
    }
    df |> 
      dplyr::select(
        "ID" = subject_id, 
        "Form" = item_group, 
        "Event" = event_name, 
        "Edit date" = edit_date_time, 
        "Reviewer" = reviewer, 
        "Time" = timestamp, 
        comment
      ) 
  })
  if(report_table == "query_data") return({
    if(report_type == "session"){
      df <- df |> 
        dplyr::mutate(
          include = (reviewer == report_reviewer & 
                       any(timestamp >= include_from_date)), 
          .by = query_id
        ) |> 
        with(df[include, ])
    }
    df |> 
      dplyr::select(
        query_id, 
        "ID" = subject_id, 
        "Form" = item_group, 
        "Item" = item, 
        "Event" = event_label, 
        "Time" = timestamp, 
        "Query" = query, 
        "Author" = reviewer,
        resolved
      )
  })
  stop("Unknown report_table '", report_table, "'.")
}


#' Create Report
#' 
#' Creates a PDF markdown report. 
#'
#' @param fileinput Character vector with the name of the file to be created.
#' @param reviewer Character vector with the name of the current reviewer. 
#' @param study_sites Character vector with the names of the sites that were reviewed.
#' @param review_df Data frame with review data to be used in the report.
#' @param query_df Data frame with query data to be used in the report.
#'
#' @return A PDF report will be created.
#' @export 
#'
create_report <- function(
    fileinput, 
    reviewer, 
    study_sites,
    review_df, query_df
    ) {
  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  tempReport <- withr::local_tempfile(fileext = ".Rmd")
  file.copy(
    app_sys("app/www/report.Rmd"), 
    tempReport, overwrite = TRUE
    )
  
  # Set up parameters to pass to Rmd document
  params <- list(
    author = reviewer,
    sites  = study_sites,
    review_table = review_df,
    queries_table = query_df
    )
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(tempReport, output_file = fileinput,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}
