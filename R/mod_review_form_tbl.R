#' review_form_tbl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_review_form_tbl_ui <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table"))
}

#' review_form_tbl Server Functions
#'
#' @noRd 
mod_review_form_tbl_server <- function(id, r, table_data, form, show_all, table_names = NULL, title = NULL){
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
