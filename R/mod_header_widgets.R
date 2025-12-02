#' Header widgets - Shiny module UI
#'
#' @inherit mod_header_widgets_server
#' @seealso [mod_header_widgets_server()]
#' 
mod_header_widgets_ui <- function(id){
  ns <- NS(id)
  tagList(
        bslib::layout_column_wrap(
          width = NULL,
          fixed_width = FALSE, 
          style = bslib::css(grid_template_columns = "1fr 1fr 1fr 3fr"),
          mod_navigate_participants_ui("navigate_participants_1"),
          shiny::uiOutput(ns("ae_box"), class = "top-widgets-ui"), 
          mod_navigate_review_ui("navigate_review_1"),
          bslib::card(
          max_height = "75px",
          plotOutput(ns("visit_figure"), height = "auto"),
          # to change the padding with css:
          class = "timeline-fig-basic"
        ),
        class = "top-widgets-custom"
    )
  )
}

#' Header widgets - Shiny module Server
#'
#' A shiny module. Used to show user information of the active user in value
#' boxes on the top of the screen.
#'
#' The value boxes in this module contain information about the active user.
#' They show the patient id, the patient status (active/inactive participant),
#' the number of adverse events (with a color code showing whether any new or
#' updated data is available), the number of forms that need a review, and a
#' timeline figure showing the number of visits that the patient performed. The
#' value box with adverse events also serves as a link to the adverse events
#' form. Furthermore, clicking on the box with forms to review will trigger
#' [mod_navigate_review_server()], opening a modal that shows the forms that
#' need review and the queries that are open of the active participant, to which
#' you can directly navigate to.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values.
#' @param rev_data common reactive values with review data.
#' @param navinfo Reactive values created with [shiny::reactiveValues()]. Used
#'   to send back information about the page change to the server, when clicking
#'   on the adverse event box.
#'
#' @seealso [mod_header_widgets_ui()]
mod_header_widgets_server <- function(
    id, 
    r, 
    rev_data, 
    navinfo,
    available_data
    ){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactivevalues(navinfo))
  stopifnot(is.reactivevalues(rev_data))
  stopifnot(is.data.frame(available_data))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    all_aes <- reactive({ 
      validate(need(r$filtered_data[["Adverse events"]], "AE data missing"))
      count_adverse_events(
        data = r$filtered_data[["Adverse events"]], 
        all_ids = unique(available_data$subject_id)
        )
      })
    
    shinyjs::onclick("ae_box", {
      navinfo$active_tab = "Common events"
      navinfo$active_form <- "Adverse events"
      navinfo$trigger_page_change <- navinfo$trigger_page_change + 1
    })
    
    all_AEs_reviewed <- reactive({
      req(rev_data$summary())
      req(r$subject_id)
      revs <- with(rev_data$summary(), reviewed[
             subject_id == r$subject_id & Form == "Adverse events"])
      !("No" %in% revs)
    })
    
    ### Outputs: 

    output[["ae_box"]] <- renderUI({
      req(inherits(all_AEs_reviewed(), "logical"), r$subject_id)
      bslib::value_box(
        title = paste0("SAEs: ", with(all_aes(), SAEs[subject_id == r$subject_id]) ), 
        value = paste0("AEs: ", with(all_aes(), AEs[subject_id == r$subject_id])),
        showcase = icon("house-medical", class = 'fa-2x'),
        theme = if(all_AEs_reviewed()) "primary" else "warning" 
      )
    })
    output[["visit_figure"]] <- renderPlot(
      {
        golem::cat_dev("plot datapoints figure\n")
        fig_timeline(
          data =  available_data[available_data$subject_id %in% r$subject_id, ]
        )
      }, 
      height = 60
    )
  })
}

## To be copied in the UI
# mod_header_widgets_ui("header_widgets_1")

## To be copied in the server
# mod_header_widgets_server("header_widgets_1")
