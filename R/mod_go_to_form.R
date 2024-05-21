#' Go to form - Shiny module UI
#' 
#' @param button_text Character vector containing the text that will be displayed on the button.
#' @param class Class of the `actionButton`. Default class is "btn-warning m2".
#'
#' @inherit mod_go_to_form_server 
#' @seealso [mod_go_to_form_server()]
#' 
mod_go_to_form_ui <- function(
    id, 
    button_text = "Go to form", 
    class = "btn-warning m2"
){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("go_to_form"), 
      button_text, 
      class = class
    )
  )
}
    
#' Go to form - Shiny module Server
#' 
#' @description A `shiny` module. Navigates to the subject and form that are mentioned 
#' in a selected table row. Designed to be used with [DT::datatable()], since 
#' these tables can provide a reactive value with the `tablerow` highlighted on click.
#' 
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param r Common reactive values. Used to send back information about the 
#' selected subject id (`r$subject_id`) to the main server. In addition, 
#' `r$filtered_subjects` is needed to verify whether an ID selected in the `tablerow` 
#' is in the active, filtered selection.  
#' @param navinfo Common reactive values. Used to send back the request for the 
#' change in form/subject id back to the server. Contains the character strings 
#' `active_form`, `active_tab`, and `trigger_page_change`. The module will create 
#' these variables if they are not yet available in `navinfo`.
#' @param navtable A reactive value. Table to look up the form and subject id to navigate to.
#' @param tablerow A reactive value. Table row associated with navtable to look 
#' up the form and subject id in. For example, if `navtable` is converted 
#' to a [DT::datatable()] object in `output$DTnavtable`, `tablerow` should be 
#' `input$DTnavtable_table_rows_selected`. See also the 
#' [`DT::datatable()` documentation](https://rstudio.github.io/DT/shiny.html). 
#' @param form_name Character vector. Name of the column containing forms. If 
#' defined, this column needs to be in the `navtable`. If this 
#' variable is `NULL`, it will default to the first variable in vars$all_forms. 
#' @param subject_id Character vector. Name of the column containing the subject 
#' identifiers. This column needs to be defined in the `navtable`.
#' @inheritParams mod_navigate_forms_server
#' 
#' @seealso [mod_go_to_form_server()]
#'
mod_go_to_form_server <- function(
    id, 
    r, 
    navinfo,
    navtable,
    tablerow,
    all_forms,
    form_name = "Form",
    subject_id = "subject_id"
    ){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.reactivevalues(navinfo))
  stopifnot(is.reactive(tablerow))
  stopifnot(is.reactive(navtable))
  stopifnot(is.character(c(form_name, subject_id)))
  stopifnot(!is.null(subject_id))
  stopifnot(is.data.frame(all_forms))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$go_to_form, {
      cat("Request to browse to selected row\n")
      req(tablerow())
      # LSA Thu Nov  9 09:49:39 2023 ------------------------------
      # Check for data/table errors and abort to prevent the entire app from crashing. 
      # Check form_name and subject_id not only at start up but within this observeEvent, 
      # because navtable() is a reactive table that might change during app usage.
      if(!is.null(form_name)){
        if(!form_name %in% names(navtable())) return(
          warning(
          "form_name '", form_name,  "' not found in the table. Go to form aborted. 
          Set form_name to NULL if selection should only take place based on subject id."
          )
        )
      }
      if(!subject_id %in% names(navtable())) return(
        warning("subject_id '", subject_id, "' not found in the table. Go to form aborted.")
      )
      
      if(length(tablerow()) > 1) warning("Multiple rows selected. Only the first row will be used.")
      selected_row <- navtable()[tablerow()[1], ]
      golem::print_dev(selected_row)
      
      row_subject <- selected_row[subject_id][[1]]
      if(is.null(row_subject) || is.na(row_subject)) return(
        warning("Invalid subject_id selected in table. ", 
                "Go to form aborted. ")
        )
      if(!row_subject %in% r$filtered_subjects) return(
        warning("subject_id in table not in list of known subject ids. 
                Go to form aborted.")
      )
      if(row_subject != r$subject_id){
        cat("patient selection changed\n")
        r$subject_id <- row_subject 
      }
      if(is.null(form_name)) {
        # if form_name is NULL, just browse to the participant's first form:
        navinfo$active_tab <- with(all_forms, main_tab[1])
        navinfo$active_form <- with(all_forms, form[1])
      } else {
        sel_form <- selected_row[[form_name]]
        navinfo$active_form <- sel_form 
        if(is.null(navinfo$active_form) || navinfo$active_form == "") return(
          warning("No valid active form selected. Go to form aborted.")
        )
        navinfo$active_tab  <- with(all_forms, main_tab[form == sel_form])
      }
      navinfo$trigger_page_change <- sum(c(navinfo$trigger_page_change, 1))
      removeModal()
    }, ignoreInit = TRUE)
    
  })
}

## To be copied in the UI
# mod_go_to_form_ui("go_to_form_1")

## To be copied in the server
# mod_go_to_form_server("go_to_form_1", r)
