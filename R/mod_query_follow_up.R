#' Add follow-up to a query - Shiny module UI
#'
#' @description A shiny Module.
#'
#'
#' @inherit mod_query_follow_up_server
#' @seealso [mod_query_follow_up_server()]
#' 
mod_query_follow_up_ui <- function(id){
  ns <- NS(id)
  bslib::card(
    id = ns("query_follow_up"),
    shiny::textAreaInput(
      inputId = ns("query_follow_up_text"),
      label = NULL,
      width = "100%",
      height = "200px",
      placeholder = "add response here"
    ),
    checkboxInput(
      ns("resolved"), 
      "resolved", 
      value = FALSE
    ),
    textOutput(ns("query_error")),
    shiny::actionButton(
      inputId = ns("query_add_follow_up"),
      label = "Respond to query"
    )
  )
}

#' Add follow-up to a query - Shiny module Server
#'
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param r Common reactiveValues. Used to access the active `subject_id` and 
#' the current reviewer's name `user_name`. 
#' @param selected_query A reactive value. Contains a character vector with the 
#' query ID, which is the unique identifier of each query.
#' @param db_path A character string with the path to the database.
#'
#' @seealso [mod_query_follow_up_server()]
mod_query_follow_up_server <- function(id, r, selected_query, db_path){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactive(selected_query))
  stopifnot(is.character(db_path))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(selected_query(), {
      is_resolved <- any(
        with(r$query_data, resolved[query_id == selected_query()]) == "Yes" 
      )
      shiny::updateCheckboxInput(inputId = "resolved", value = is_resolved)
      shiny::updateTextAreaInput(
        inputId = "query_follow_up_text", 
        placeholder = ifelse(
          is_resolved, 
          "query is resolved", 
          "add response here"
        )
      )
      if(is_resolved){
        shinyjs::disable("query_follow_up") 
      } else{
        shinyjs::enable("query_follow_up") 
      }
    })
    query_save_error <- reactiveVal(FALSE)
    observeEvent(input$query_add_follow_up, {
      req(input$query_follow_up_text, r$user_name, selected_query())
      req(selected_query() %in% r$query_data$query_id)
      query_save_error(FALSE)
      golem::cat_dev("Query FU text to add: ", input$query_follow_up_text, "\n")
      ts <- time_stamp()
      
      updated_query <- db_get_query(db_path, selected_query()) |> 
        db_slice_rows(slice_vars = "timestamp", group_vars = "query_id") |> 
        dplyr::distinct(query_id, type, subject_id, item_group, item, event_label, n)
      updated_query <- updated_query |> 
        dplyr::mutate(
          "timestamp"     = ts, 
          "n"             = n + 1,
          "reviewer"      = paste0(r$user_name," (", r$user_role, ")"),
          "query"         = input$query_follow_up_text,
          "resolved"      = ifelse(input$resolved, "Yes", "No"),
          `resolved_date` = if(input$resolved) ts else NA_character_,
          "edit_reason"   = NA_character_
        )
      golem::print_dev(updated_query)
      
      # Update queries and selected queries data:
      db_save(data = updated_query, db_path = db_path, db_table = "query_data")
      #verify if query update was successful:
      query_in_db <- db_get_query(
        db_path, query_id = updated_query$query_id, n = updated_query$n
      )
      query_in_db <- unique(query_in_db[names(updated_query)])
      if(identical(updated_query, query_in_db)){
        r$query_data <- dplyr::bind_rows(r$query_data, updated_query) 
        if(updated_query$resolved == "Yes"){
          r$query_data <- r$query_data |> 
            dplyr::rows_update(
              updated_query[c("query_id", "resolved", "resolved_date")], 
              by = "query_id"
            )
        }
      }
      query_in_memory <- dplyr::as_tibble(
        r$query_data[nrow(r$query_data), names(updated_query)]
      )
      query_save_error(any(
        !identical(updated_query, query_in_db),
        !identical(query_in_db, query_in_memory)
      ))
      
      if(query_save_error()){
        return({
          showNotification(
            paste0("An error occurred while trying to save the data.",
                   "Try again or contact an administrator if the ",
                   "problem persists. ",
                   "Resynching in-memory data with database.."),
            id = ns("query_save_error"),
            type = "error",
            duration = 5
          )
          r$query_data <- collect_query_data(db_path)
        })
      }
      
      updateTextInput(inputId = "query_follow_up_text", value = "")
      
      shiny::updateCheckboxInput(
        inputId = "resolved",
        session = session, 
        value = FALSE
      )
    })
    
    output[["query_error"]] <- renderText({
      req(input$query_add_follow_up)
      validate(
        need(selected_query(), "Select a query to follow-up"),
        need(selected_query() %in% r$query_data$query_id, 
             "Query ID unknown. Verify the database"),
        need(input$query_follow_up_text, "Follow-up message missing"),
        need(r$user_name, "User name missing. Cannot save query anonymously.")
      )
    })
  })
}

## To be copied in the UI
# mod_query_follow_up_ui("query_follow_up_1")

## To be copied in the server
# mod_query_follow_up_server("query_follow_up_1")
