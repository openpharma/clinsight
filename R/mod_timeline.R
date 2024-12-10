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
#' A shiny module. Creates an interactive timeline. 
#' Used to provide a quick overview of adverse events, severe adverse events, 
#' and study activities such as Investigational Product administration. 
#' Helpful to judge whether for example an event is related to an Investigational 
#' Product.
#'
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param r Common reactive values. Used to access the data frames `review_data`, 
#' `filtered_tables`, and the active `subject_id`. `review_data` will be used to 
#' minimize data points on the timeline that are already reviewed.
#' @param form A character vector, the form in which the timeline needs to be embedded.
#' Currently, only the form 'Adverse events' is supported.
#'
#' @seealso [mod_timeline_ui()], [mod_common_forms_ui()], [mod_common_forms_server()]
mod_timeline_server <- function(id, r, form, treatment_label){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    timeline_data <- reactive({
      req(form == "Adverse events")
      get_timeline_data(
        r$filtered_data, 
        r$filtered_tables, 
        treatment_label = treatment_label
      )
    })
    timeline_data_active <- reactive({
      req(timeline_data())
      # join below is on both form_repeat and item_group. Join by item_group 
      # is to prepare for future timelines when other forms will also be included. 
      review_active <- with(
        r$review_data, 
        r$review_data[item_group == form & subject_id == r$subject_id, ]
        ) |> 
        dplyr::mutate(
          needs_review = any(reviewed == "No"),
          .by = c(form_repeat, item_group)
        ) |> 
        dplyr::distinct(subject_id, form_repeat, item_group, needs_review)
      
      df <- with(timeline_data(), timeline_data()[subject_id == r$subject_id, ]) |> 
        dplyr::left_join(review_active, by = c("subject_id", "form_repeat", "item_group")) |> 
        dplyr::mutate(
          content = ifelse(is.na(needs_review), content,
                           ifelse(needs_review, content, NA_character_)),
          style = ifelse(is.na(needs_review), style,
                         ifelse(needs_review, style,
                                paste0(style, "; line-height: 0.1; border-radius: 20px;")
                         ))
        )
      df
    })

    if(form != "Adverse events"){
      shinyjs::hide("timeline")
    }
    
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
      )
    })
    
  })
}

## To be copied in the UI
# mod_timeline_ui("timeline_1")

## To be copied in the server
# mod_timeline_server("timeline_1")
