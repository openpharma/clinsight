#' Review form - Shiny module UI
#'
#'
#' @inherit mod_review_forms_server 
#' @seealso [mod_review_forms_server()]
#' 
mod_review_forms_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::tabsetPanel(
      id = ns("confirm_review_tabs"),
      type = "hidden",
      tabPanel("empty_panel", ""),
      tabPanel(
        "show_checkbox",
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::tooltip(
            checkboxInput(
              inputId = ns("form_reviewed"),
              label = "Reviewed",
              value = FALSE 
            ),
            "Mark as reviewed",
            placement = "bottom"
          ),
          bslib::tooltip(
            shinyWidgets::prettySwitch(
              inputId = ns("add_comment"), 
              label = NULL,  
              status = "primary",
              slim = TRUE,
              value = FALSE
            ), 
            "Add a comment",
            placement = "right", options = list(`offset` = "0,-25")
          )
        ),
        conditionalPanel(
          condition = "input.add_comment === true",
          ns = NS(id),
          shiny::textAreaInput(
            inputId = ns("review_comment"), 
            label = NULL
          )
        ),
        bslib::layout_columns(
          col_widths = c(11, 12),
          shiny::actionButton(
            inputId = ns("save_review"), 
            "Save", 
            icon = icon("floppy-disk"),
            class = "btn-primary m2"
          ),
          textOutput(ns("save_review_error"))
        )
      )
    )
  )
}

#' Review form - Shiny module Server
#'
#' @description A `shiny` module. Used to mark data as reviewed and add comments
#'   to data of a specific form/page in the database and in the internal
#'   application data.
#'
#'   This module is used to display whether the form that is actively being
#'   displayed in the application has any values that are newly entered or
#'   updated since the last review session. In addition, if a page contains new
#'   data, the user can mark all data in the form as being reviewed and can
#'   (optionally) add a comment to this review action. The data will be saved in
#'   a database. All review activity is stored with an audit-trail, with date/time 
#'   stamps and with the reviewer's name. 
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access subject_id, user_name,
#'   review_data, and status_data. The module can change data in the review_data
#'   and to status_data data frames.
#' @param active_form Reactive value. Used to determine which form is active and
#'   whether that form needs review.
#' @param active_tab Reactive value containing the active tab. Needed to hide
#'   the review controls if non-relevant tabs are selected (tabs that do not
#'   contain common forms or study forms).
#' @param db_path Character string with the file path to the database.
#'
#' @seealso [mod_review_forms_ui()]
#' 
mod_review_forms_server <- function(
    id, 
    r, 
    active_form, 
    active_tab, 
    db_path
){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactive(active_form))
  stopifnot(is.reactive(active_tab))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    review_data_active <- reactive({
      df <- r$review_data |>
        dplyr::filter(subject_id == r$subject_id, 
                      item_group == active_form()) |> 
        dplyr::distinct(subject_id, item_group, edit_date_time, reviewed, comment, status)
      #!! below selects the latest edit_date_time; usually only one row will remain by then since there are no items displayed here.
      if(nrow(df)== 0) return(df)
      df |> 
        dplyr::filter(edit_date_time == max(as.POSIXct(edit_date_time)))
    }) 
    
    observeEvent(c(active_form(), r$subject_id), {
      cat("Update confirm review button\n\n\n")
      req(r$review_data)
      golem::cat_dev("review_data_active:\n")
      golem::print_dev(review_data_active())
      if(nrow(review_data_active()) == 0){ 
        cat("No review data found for Subject id: ", r$subject_id, 
            " and group: ", active_form(), "\n") 
        review_status <- "No"
        review_comment <- ""
      } else {
        # in the rare case of multiple rows selected, the unique reviewed label 
        # will be selected below. if there are multiple labels ("Yes and No"), 
        # it will give a warning. This would be rare since it would mean a datapoint with the same edit date-time was reviewed but another one was not. 
        # probably better to use defensive coding here to ensure the app does not crash in that case. However we need to define which review status we need to select
        # in this case get the reviewed = "No"
        review_status  <- unique(review_data_active()$reviewed)
        review_comment <- unique(review_data_active()$comment)
        if(length(review_status) != 1) warning("multiple variables in review_status, namely: ", 
                                               review_status, "Verify data.")
      }
      
      updateCheckboxInput(
        inputId = "form_reviewed",
        value = (review_status == "Yes")
      )
      
      shinyWidgets::updatePrettySwitch(
        session = session,
        inputId = "add_comment",
        value = (review_comment != "")
      )
      updateTextAreaInput(
        inputId = "review_comment",
        value = review_comment
      )
    })
    
    user_allowed_to_review <- reactive({
      isFALSE(is.null(r$user_name) || r$user_name == "")
    })
    
    role_allowed_to_review <- reactive({
      get_roles_from_config()[r$user_role] %in% get_golem_config("allow_to_review")
    })
    
    enable_any_review <- reactive({
      all(c(
        user_allowed_to_review(),
        role_allowed_to_review(),
        nrow(review_data_active()) != 0
      ))
    })
    
    enable_save_review <- reactive({
      req(
        review_data_active(), 
        is.logical(input$form_reviewed), 
        is.logical(enable_any_review()),
        ) 
      if(!enable_any_review()) return(FALSE)
      any(c(
        unique(review_data_active()$reviewed) == "No"  & input$form_reviewed, 
        unique(review_data_active()$reviewed) == "Yes" & !input$form_reviewed
      ))
    })
    
    observeEvent(c(enable_any_review(), enable_save_review()), {
      req(is.logical(enable_any_review()), is.logical(enable_save_review()))
      shinyjs::toggleState("form_reviewed", enable_any_review())
      if(enable_save_review()){
        shinyjs::enable("save_review")
        shinyjs::enable("add_comment")
        shinyjs::enable("review_comment")
      } else{
        shinyjs::disable("save_review")
        shinyjs::disable("add_comment")
        shinyjs::disable("review_comment")
      }
    })
    
    ## only show review panel with common events and study data tabs:
    observeEvent(active_tab(), {
      check_box_tab <- "empty_panel"
      cat("main tabs: ", active_tab(), "\n\n")
      if(active_tab() %in% c("Common events", "Study data")){
        check_box_tab <- "show_checkbox"
      } 
      shiny::updateTabsetPanel(
        session = session,
        inputId = "confirm_review_tabs",
        selected = check_box_tab
      )
    })
    
    review_save_error <- reactiveVal(FALSE)
    observeEvent(input$save_review, {
      req(is.logical(input$form_reviewed), review_data_active())
      req(enable_save_review())
      review_save_error(FALSE)
      golem::cat_dev("Save review status reviewed:", input$form_reviewed, "\n")
      
      review_row <- review_data_active() |> 
        dplyr::distinct(subject_id, item_group) |> 
        dplyr::mutate(
          reviewed    = if(input$form_reviewed) "Yes" else "No",
          comment     = ifelse(is.null(input$review_comment), "", input$review_comment),
          reviewer    = paste0(r$user_name, " (", r$user_role, ")"),
          timestamp   = time_stamp(),
          status      = if(input$form_reviewed) "old" else "new"
        ) 
      
      golem::cat_dev("review row to add:\n")
      golem::print_dev(review_row)
      
      cat("write review progress to database\n")
      db_save_review(
        review_row, 
        db_path = db_path,
        # More tables can be added here if needed, to track process of 
        # individual reviewers in individual tables:
        tables = "all_review_data" 
      )
      
      review_row_db <- db_get_review(
        db_path, subject = review_row$subject_id, form = review_row$item_group
        )
      review_row_db <- unique(review_row_db[names(review_row)])
      if(identical(review_row_db, review_row)){
        cat("Update review data and status in app\n")
        r$review_data <- r$review_data |> 
          dplyr::rows_update(review_row, by = c("subject_id", "item_group"))
      }
      
      review_row_memory <- review_row |> 
        dplyr::left_join(r$review_data, by = names(review_row)) 
      review_row_memory <- unique(review_row_memory[names(review_row)])
      
      review_save_error(any(
        !identical(review_row_db, review_row),
        !identical(review_row_memory, review_row_db)
      ))
      
      if(review_save_error()){
        return({
          showNotification(
            paste0("An error occurred while trying to save the data. ",
                   "Try again or contact an administrator if the ",
                   "problem persists. ", 
                   "Resynching in-memory data with database.."), 
            duration = 5, 
            id = ns("review_save_error"),
            type = "error"
          )
          r$review_data <- db_slice_rows(db_path, db_table = "all_review_data")
        })
      }
      showNotification("Input saved successfully", duration = 1, type = "message") 
    })
    
    output[["review_header"]] <- renderText({active_form()})
    
    output[["save_review_error"]] <- renderPrint({
      validate(need(
        role_allowed_to_review(), 
        paste0("Review not allowed for a '", r$user_role, "'.")
        ))
      validate(need(
        nrow(review_data_active()) != 0,
        "Nothing to review"
      ))
      validate(need(
        user_allowed_to_review(), 
        "No user name found. Cannot save review"
      ))
      validate(need(
        !review_data_active()$reviewed == "Yes",
        "Form already reviewed"
      ))
      validate(need(input$form_reviewed, "Requires review"))
    })
    
  })
}

## To be copied in the UI
# mod_review_forms_ui("review_controls_1")

## To be copied in the server
# mod_review_forms_server("review_controls_1", r)
