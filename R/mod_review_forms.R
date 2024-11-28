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
#'   a database. All review activity is stored with an audit-trail, with
#'   date/time stamps and with the reviewer's name.
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
#' @param review_required_data a data frame with information about whether
#'   review is mandatory for each form. Should contain the columns `item_group`
#'   and `review_required`.
#' @param db_path Character string with the file path to the database.
#'
#' @seealso [mod_review_forms_ui()]
#' 
mod_review_forms_server <- function(
    id, 
    r, 
    active_form, 
    active_tab, 
    review_required_data,
    db_path
){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactive(active_form))
  stopifnot(is.reactive(active_tab))
  stopifnot(is.data.frame(review_required_data))
  if(!all(c("item_group", "review_required") %in% names(review_required_data))){
    stop("Either the 'study_forms or 'review_required' column is missing ", 
         "from the review_required data frame")
  }
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    review_data_active <- reactive({
      r$review_data |>
        dplyr::filter(subject_id == r$subject_id, 
                      item_group == active_form())
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
        review_status <- with(review_data_active(), reviewed[edit_date_time == max(as.POSIXct(edit_date_time))]) |> unique()
        review_comment <- with(review_data_active(), comment[edit_date_time == max(as.POSIXct(edit_date_time))]) |> unique()
        if(length(review_status) != 1) warning("multiple variables in review_status, namely: ", 
                                               review_status, "Verify data.")
      }
      
      updateCheckboxInput(
        inputId = "form_reviewed",
        value = identical(review_status, "Yes")
      )
      
      shinyWidgets::updatePrettySwitch(
        session = session,
        inputId = "add_comment",
        value = !identical(review_comment, "")
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
    
    review_required <- reactive({
      req(active_form(), review_required_data)
      with(
        review_required_data, 
        review_required[item_group == active_form()]
      ) %||% TRUE
    })
    
    enable_any_review <- reactive({
      all(c(
        review_required(),
        user_allowed_to_review(),
        role_allowed_to_review(),
        nrow(review_data_active()) != 0
      ))
    })
    
    enable_save_review <- reactive({
      req(
        review_data_active(), 
        is.logical(input$form_reviewed), 
        is.logical(enable_any_review())
        ) 
      if(!enable_any_review()) return(FALSE)
      any(c(
        unique(with(review_data_active(), reviewed[edit_date_time == max(as.POSIXct(edit_date_time))])) == "No"  & input$form_reviewed, 
        unique(with(review_data_active(), reviewed[edit_date_time == max(as.POSIXct(edit_date_time))])) == "Yes" & !input$form_reviewed
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
      
      review_records <- review_data_active()["id"] |> 
        dplyr::mutate(
          reviewed    = if(input$form_reviewed) "Yes" else "No",
          comment     = ifelse(is.null(input$review_comment), "", input$review_comment),
          reviewer    = paste0(r$user_name, " (", r$user_role, ")"),
          timestamp   = time_stamp(),
          status      = if(input$form_reviewed) "old" else "new"
        ) 
      
      golem::cat_dev("review records to add:\n")
      golem::print_dev(review_records)
      
      cat("write review progress to database\n")
      db_save_review(
        review_records, 
        db_path = db_path,
        table = "all_review_data" 
      )
      
      updated_rows_db <- db_get_review(
        db_path, id = review_records$id
        ) |> 
        dplyr::select(dplyr::all_of(names(review_records)))
      
      review_records_db <- updated_rows_db |> 
        # Within a form, only items with a changed review state are updated and 
        # contain the new (current) time stamp. 
        dplyr::filter(timestamp == review_records$timestamp[1])
      if(isTRUE(all.equal(review_records_db, review_records, check.attributes = FALSE))){
        cat("Update review data and status in app\n")
        r$review_data <- r$review_data |> 
          dplyr::rows_update(review_records, by = "id")
      }
      
      updated_records_memory <- with(r$review_data, r$review_data[
        id %in% review_records$id &
        timestamp == review_records$timestamp[1],
        names(review_records_db)
      ])
      
      review_save_error(any(
        !isTRUE(all.equal(review_records_db, review_records, check.attributes = FALSE)),
        !isTRUE(all.equal(updated_records_memory, review_records_db, check.attributes = FALSE))
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
          r$review_data <- db_get_table(db_path, db_table = "all_review_data")
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
        review_required(), 
        "Review not required"
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
        !unique(with(review_data_active(), reviewed[edit_date_time == max(as.POSIXct(edit_date_time))])) == "Yes",
        "Form already reviewed"
      ))
      validate(need(input$form_reviewed, "Requires review"))
    })
    
    shiny::exportTestValues(
      review_save_error = review_save_error()
    )
  })
}

## To be copied in the UI
# mod_review_forms_ui("review_controls_1")

## To be copied in the server
# mod_review_forms_server("review_controls_1", r)
