#' Review forms table - Shiny module UI
#'
#' @inherit mod_review_form_tbl_server
#' @seealso [mod_review_form_tbl_server()]
#'
mod_review_form_tbl_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table"))
}

#' Review forms table - Shiny module Server
#'
#' Shiny module. Used to handle the logic and presentation of form review data.
#'
#' This module handles the logic associated with the form review tables. It
#' handles all the logic associated with the table check boxes and the observers
#' needed to keep the browser table in-synch with the server table.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access the data frames
#'   `review_data`, `filtered_tables`, and the active `subject_id`.
#'   `review_data` will be used to determine which rows will be displayed in
#'   bold and, for the form Adverse events, which timeline data should be
#'   highlighted.
#' @param table_data Common reactive value. Used to manage the server data
#'   displayed in the DataTable.
#' @param form A character string with the name of the form to display.
#' @param show_all Common reactive value, a logical indicating whether all
#'   records should be displayed.
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @param title An optional character vector. If provided, will be used within
#'   [datatable_custom()], as the title for the table.
#'
#' @seealso [mod_review_form_tbl_ui()], [mod_common_forms_ui()],
#'   [mod_common_forms_server()], [mod_study_forms_ui()],
#'   [mod_study_forms_server()]
#' 
mod_review_form_tbl_server <- function(
    id,
    r,
    table_data,
    form,
    show_all,
    table_names = NULL,
    title = NULL
){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactive(table_data))
  stopifnot(is.character(form), length(form) == 1)
  stopifnot(is.reactive(show_all))

  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    reload_data <- reactiveVal(0)
    
    ############################### Observers: #################################
    
    observe({
      reload_data(reload_data() + 1)
      session$userData$update_checkboxes[[form]] <- NULL
      session$userData$review_records[[form]] <- data.frame(id = integer(), reviewed = character())
    }) |> 
      bindEvent(r$subject_id, r$review_data)
    
    observeEvent(session$userData$update_checkboxes[[form]], {
      reload_data(reload_data() + 1)
      checked <- session$userData$update_checkboxes[[form]]
      
      df <- table_data() |> 
        dplyr::mutate(o_reviewed = dplyr::if_else(subject_id == r$subject_id, 
                                                  lapply(o_reviewed, modifyList, list(updated = checked)),
                                                  o_reviewed))
      table_data(df)
    })
    
    observeEvent(input$table_review_selection, {
      # Update review values for session's user data
      session$userData$update_checkboxes[[form]] <- NULL
      session$userData$review_records[[form]] <-
        update_review_records(
          session$userData$review_records[[form]],
          input$table_review_selection[, c("id", "reviewed")],
          subset(r$review_data, subject_id == r$subject_id & item_group == form,
                 c("id", "reviewed"))
        )
      
      # Update the table's data reactive
      df <- update_tbl_data_from_datatable(
        table_data(), 
        input$table_review_selection
      )
      table_data(df)
    })
    
    # Any time the data in the form table is updated, "show all" is toggled,
    # or the subject being viewed is changed, the server data for the datatable
    # needs to be updated
    observe({
      req(!is.null(show_all()))
      req(table_data())
      DT::dataTableAjax(table_proxy$session, 
                        subset(table_data(), show_all() | subject_id == r$subject_id), 
                        rownames = FALSE,
                        outputId = table_proxy$rawId)
    })
    # Any time the review table is updated, "show all" is toggled, or the
    # subject being viewed is changed, the datatable should be reloaded to show
    # the new data
    observeEvent(reload_data(), {
      req(!is.null(show_all()))
      req(table_data())
      DT::reloadData(table_proxy)
    }, ignoreInit = TRUE)
    
    observeEvent(r$subject_id, {
      req(table_data())
      reload_data(reload_data() + 1)
      df <- table_data() |> 
        dplyr::mutate(o_reviewed = Map(\(x, y) modifyList(x, list(updated = NULL, disabled = y)), o_reviewed, subject_id != r$subject_id))
      table_data(df)
    })
    
    observeEvent(show_all(), {
      req(table_data())
      reload_data(reload_data() + 1)
      index <- match("subject_id", colnames(table_data())) - 1
      if (show_all()) {
        DT::showCols(table_proxy, index)
      } else {
        DT::hideCols(table_proxy, index)
      }
    })
    
    ############################### Outputs: ###################################
    
    output[["table"]] <- DT::renderDT({
      req(r$filtered_data[[form]])
      datatable_custom(
        isolate(subset(table_data(), show_all() | subject_id == r$subject_id)), 
        rename_vars = c("Review Status" = "o_reviewed", table_names), 
        rownames= FALSE,
        title = title,
        escape = FALSE,
        selection = "none",
        callback = checkbox_callback,
        options = list(
          columnDefs = list(
            list(
              targets = "o_reviewed",
              orderable = FALSE,
              render = checkbox_render
            ),
            list(
              targets = "subject_id",
              visible = isolate(show_all())
            )),
          rowCallback = row_callback
        ))
    })
    table_proxy <- DT::dataTableProxy("table")
    
  })
}
    
## To be copied in the UI
# mod_review_form_tbl_ui("review_form_tbl_1")
    
## To be copied in the server
# mod_review_form_tbl_server("review_form_tbl_1")
