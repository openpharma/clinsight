#' Navigate forms - Shiny module UI
#' 
#'
#' @description Shiny UI module containing the UI needed to navigate between 
#' forms. The UI contains a `back` and `forward` which changes the active form. 
#' In addition, the active form is displayed either in bold or in normal font 
#' face, depending on whether the form contains any non-reviewed data.
#'
#' @inherit mod_navigate_forms_server 
#' @seealso [mod_navigate_forms_server()]
#' 
mod_navigate_forms_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(1,10,1), 
      shiny::actionLink(ns("form_previous"), label = NULL, icon = icon("angle-left")),
      htmlOutput(ns("form_name")),
      shiny::actionLink(ns("form_next"), label = NULL, icon = icon("angle-right")),
      fillable = TRUE,
      gap = "0%",
      height = "10px"
    )
  )
}
    
#' Navigate forms - Shiny module Server
#' 
#' Shiny server module to navigate between forms. Navigation is done with a 
#' `back` and `forward` button in the UI module.
#' 
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param navinfo Reactive values created with [shiny::reactiveValues()]. Used 
#' to send back information about the page change to the server. 
#' @param forms_to_review Reactive value. Contains a character vector with the forms 
#' that need review for the active/selected ID. Will be used to change the font 
#' face of the form name to bold if review of that form is needed.
#' @param all_forms A data frame containing two columns: a column named "form" with 
#' all the form to navigate through in the correct order, and a column named 
#' "main_tab", showing the name of the main_tab in which the form can be found. 
#' 
#' @seealso [mod_navigate_forms_ui()]
#' 
mod_navigate_forms_server <- function(
    id, 
    navinfo,
    forms_to_review, 
    all_forms
    ){
  stopifnot(is.reactivevalues(navinfo))
  stopifnot(is.reactive(forms_to_review))
  stopifnot(is.data.frame(all_forms))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
   
    update_navinfo <- function(new_number, navinfo){
      stopifnot({is.reactivevalues(navinfo)})
      if(is.null(new_number) || !is.numeric(new_number)) return(
      warning("Form number is missing or not numeric. 
              Are all form names correct, as described in the metadata? 
              Page change aborted")
      )
      golem::cat_dev("Change form in main server. Old form: ", navinfo$active_form, "\n")
      navinfo$active_form <- with(all_forms, form[new_number])
      navinfo$active_tab <- with(all_forms, main_tab[new_number])
      navinfo$trigger_page_change <- navinfo$trigger_page_change + 1
    }
    
    observeEvent(input$form_next, {
      req(navinfo$active_form)
      old_number <- match(navinfo$active_form, all_forms$form)
      new_number <- old_number %% length(all_forms$form) + 1
      update_navinfo(new_number, navinfo)
    })
    
    observeEvent(input$form_previous, {
      req(navinfo$active_form)
      old_number <- match(navinfo$active_form, all_forms$form)
      new_number <- ifelse(old_number == 1, length(all_forms$form), 
                           old_number - 1) 
      update_navinfo(new_number, navinfo)
    })
    
    output[["form_name"]] <- renderText({
      req(navinfo$active_form)
      form <- navinfo$active_form
      if(navinfo$active_form %in% forms_to_review()) form <- tags$b(navinfo$active_form)
      paste0("<center>", form, "</center>")
    })
  })
}

## To be copied in the UI
# mod_navigate_forms_ui("navigate_forms_1")

## To be copied in the server
# mod_navigate_forms_server("navigate_forms_1")
