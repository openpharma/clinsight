#' Interactive timeline - Shiny module UI
#'
#'
#' @inherit mod_timeline_server 
#' 
mod_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    timevis::timevisOutput(ns("timeline"))
  )
}

#' Interactive timeline - Shiny module Server
#'
#' A shiny module. Creates an interactive timeline. Used to provide a quick
#' overview of adverse events, severe adverse events, and study activities such
#' as Investigational Product administration. Helpful to judge whether for
#' example an event is related to an Investigational Product.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @inheritParams mod_common_forms_server
#'
#' @seealso [mod_timeline_ui()], [mod_common_forms_ui()],
#'   [mod_common_forms_server()]
mod_timeline_server <- function(
    id, 
    form, 
    form_review_data, 
    timeline_data,
    active_subject
    ){
  stopifnot(is.character(form), length(form) == 1)
  stopifnot(is.reactive(timeline_data))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    timeline_data_active <- reactive({
      # join below is on both form_repeat and item_group. Join by item_group 
      # is to prepare for future timelines when other forms will also be included. 
      review_active <- form_review_data()[form_review_data()$subject_id == active_subject(), ] |> 
        dplyr::mutate(
          needs_review = any(reviewed == "No"),
          .by = c(form_repeat, item_group)
        ) |> 
        dplyr::distinct(subject_id, form_repeat, item_group, needs_review)
      
      df <- with(timeline_data(), timeline_data()[subject_id == active_subject(), ]) |> 
        dplyr::left_join(review_active, by = c("subject_id", "form_repeat", "item_group")) |> 
        dplyr::mutate(
          className = ifelse(
            is.na(needs_review), 
            className,
            ifelse(needs_review, className, "bg-secondary")
            )
        )
      df
    }) |> 
      bindEvent(form_review_data(), timeline_data(), active_subject())

    if(form != "Adverse events"){
      shinyjs::hide("timeline")
    }
    
    observeEvent(input$timeline_selected, {
      timevis::centerItem("timeline", input$timeline_selected)
    })
    
    output[["timeline"]] <-  timevis::renderTimevis({
      req(form == "Adverse events")
      timevis::timevis(
        data = timeline_data_active(),
        groups = timeline_data_active() |>
          dplyr::mutate(
            "id" = .data[["group"]],
            order = as.numeric(.data[["group"]]),
            "content" = .data[["group"]]) |>
          dplyr::distinct(id, content, order),
        options = list(zoomable = FALSE)
      ) |> 
        htmlwidgets::onRender("timelineRedrawCustom")
    }) 
  })
}

## To be copied in the UI
# mod_timeline_ui("timeline_1")

## To be copied in the server
# mod_timeline_server("timeline_1")
