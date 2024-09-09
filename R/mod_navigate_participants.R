#' Navigate participants - Shiny module UI
#' 
#' @inherit mod_navigate_participants_server  
#' @seealso [mod_navigate_participants_server()] for the server function.
#' 
mod_navigate_participants_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("subject_info"), class = "top-widgets-ui")
  )
}
    
#' Navigate participants - Shiny module Server
#' 
#' A `shiny` module. Used to show participant information in a 
#' [bslib::value_box()]. By clicking on the [bslib::value_box()], additional 
#' participant information will be shown, as well as a selection menu to select 
#' a different subject. Once the subject is changed, the active `subject_id` will 
#' be changed in the application. 
#'
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param r Common `reactiveValues`. Used to access `filtered_tables$General`, 
#' containing a data frame with general data to be displayed in the participant 
#' selection modal. 
#' In addition, it will be used to access the list of `filtered_subjects` 
#' (character vector), and the currently active `subject_id` (character string). 
#' The only parameter that the module will change, if requested by the user, 
#' is `subject_id`. 
#'
#' @seealso [mod_navigate_participants_ui()] for the UI function
#' 
mod_navigate_participants_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    modal_nav_participants <- function(subjects, subj_selected){
      modalDialog(
        htmlOutput(ns("status")),
        HTML("<br><br>"),
        bslib::card_body(
          bslib::layout_columns(
            fill = FALSE,
            col_widths = c(2,8, 2),
            shiny::actionButton(
              ns("subj_previous"), 
              label = NULL, 
              icon = icon("backward-step"), 
              class = "btn-primary m2"
            ),
            selectInput(
              inputId = ns("participant_selection"),
              label = NULL,
              choices = subjects,
              selected = subj_selected
            ),
            shiny::actionButton(
              ns("subj_next"), 
              label = NULL, 
              icon = icon("forward-step"), 
              class = "btn-primary m2"
            )
          )
        ),
        footer = bslib::layout_columns(
          col_widths = c(6,6), 
          shiny::actionButton(
            inputId = ns("subj_apply"), 
            label = "Apply",
            class = "btn-warning m2"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        fade = FALSE
      )
    }
    
    active_subject <- reactiveVal()
    observeEvent(active_subject(), {
      req(active_subject())
      updateSelectInput(
        session = session, 
        inputId = "participant_selection", 
        selected = active_subject()
      )
    })
    
    shinyjs::onclick("subject_info", {
      active_subject(r$subject_id)
      showModal(modal_nav_participants(subjects = r$filtered_subjects, 
                                       subj_selected = r$subject_id))
    })
    
    observeEvent(input$subj_next, {
      golem::cat_dev("increase participant number\n")
      subject_number <- match(input$participant_selection, r$filtered_subjects) + 1
      subject_number <- pmin(length(r$filtered_subjects), subject_number)
      active_subject(r$filtered_subjects[subject_number])
    })
    
    observeEvent(input$subj_previous, {
      golem::cat_dev("decrease participant number\n")
      subject_number <- match(input$participant_selection, r$filtered_subjects) - 1
      subject_number <- pmax(1, subject_number)
      active_subject(r$filtered_subjects[subject_number])
    })
    
    observeEvent(input$subj_apply, {
      r$subject_id <- input$participant_selection
      removeModal()
    })
    
    general_info_missing_error <- reactive({
      if(is.null(r$filtered_tables$General)) {
        return("Warning: No general information found in the database.")
      }
      if(!r$subject_id %in% with(r$filtered_tables$General, subject_id) ) {
        return(
          paste0("Warning: no general information found for subject ", r$subject_id)
        )
      }
    })
    
    output[["status"]] <- renderText({
      req(input$participant_selection)
      if(!is.null(general_info_missing_error())) return(HTML(general_info_missing_error()))
      df <- r$filtered_tables$General |> 
        dplyr::filter(subject_id == input$participant_selection)
      df$status_label
    })
    
    subject_info <- reactive({
      if(!is.null(general_info_missing_error())) {
        list(
          pt_info = HTML("<i>! Data missing</i>"),
          status_icon = icon("circle-question", class = 'fa-2x')
        )
      } else{
        active_pt_info <- r$filtered_tables$General |> 
          dplyr::filter(subject_id == r$subject_id)
        list(
          pt_info = paste0(active_pt_info$Sex, ", ", active_pt_info$Age, "yrs."),
          status_icon = switch(
            active_pt_info$status, 
            Enrolled = icon("user-check", class = 'fa-2x'),
            Unknown  = icon("circle-question", class = 'fa-2x'),
            `Screen failure` = icon("user-slash", class = 'fa-2x'),
            icon("user-slash", class = 'fa-2x')
          )
        )
      }
    })
    
    output[["subject_info"]] <- renderUI({
      bslib::value_box(
        title = gsub("IME-", "", as.character(unique(r$subject_id)[1])),
        value = subject_info()$pt_info,
        showcase = subject_info()$status_icon, 
        max_height = "100px",
        theme_color = "primary"
      )
    }) 
    
  })
}

## To be copied in the UI
# mod_navigate_participants_ui("navigate_participants_1")

## To be copied in the server
# mod_navigate_participants_server("navigate_participants_1")
