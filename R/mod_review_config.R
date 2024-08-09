#' Review configuration - Shiny module UI
#' 
#' @inherit mod_review_config_server 
#' 
#' @seealso [mod_review_config_server()]
#'
mod_review_config_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::actionButton(
      inputId = ns("config_review"), 
      label = "Settings", 
      icon = icon("gears")
    )
  )
}

#' Review configuration - Shiny module Server
#' 
#' A Shiny module. Used to set the desired review configuration for the active 
#' user. 
#' 
#' When the module is called from the main server, it will show options to 
#' select regions and sites of interest to the user. The user sets these 
#' filters at the beginning of their session, to select the study sites that need 
#' review. After selecting the appropriate filter settings and confirming the 
#' review by clicking on the confirm review button, the internal data sets will 
#' be filtered on the selected filter settings and only the selected 
#' data will be shown in the application. 
#'
#' @param id Character string, used to connect the module UI with the module Server. 
#' @param r Common reactiveValues. Used to pass on filtered data and filtered subjects 
#' (based on selected sites/regions) to the main server. Expects to contain 
#' `r$filtered_data`, `r$filtered_tables`, `r$filtered_subjects` and `r$subject_id` (the '
#' active/current subject id'). The latter is needed because the `r$subject_id` 
#' needs to be set to the first ID in the filtered selection to prevent a 
#' non-selected subject_id to be active. 
#' @param app_data List of data frames with the app data.
#' @param app_tables List of data frames with the app data in wide table format.
#' @param sites A data frame with columns "site_code", with all unique site 
#' identifiers, and "region", the region of the study site.
#' @param subject_ids Character vector containing all subject ids. Used for 
#' filtering subject ids. Using this string so that the correct order will be 
#' retained. Dependency could be removed in future version.
#' 
#' @seealso [mod_review_config_ui()]
#' 
mod_review_config_server <- function(
    id, 
    r, 
    app_data,
    app_tables,
    sites,
    subject_ids
){
  moduleServer( id, function(input, output, session){
    stopifnot(is.reactivevalues(r))
    stopifnot(is.data.frame(sites))
    stopifnot(is.character(subject_ids))
    ns <- session$ns
    sites_in_appdata <- get_unique_vars(app_data, "site_code")$site_code
    if(!all(sites$site_code %in% sites_in_appdata)){
      warning("Not all sites are found in the appdata. 
              This might produce unexpected results")
    }
    
    # These values need to be stored so that the modal will remember the 
    # settings when opened again: 
    modvars <- reactiveValues(
      region_selection       = unique(sites$region),
      site_selection         = sites$site_code,
      site_selection_choices = sites$site_code,
    )
    review_modal <- function(){
      modalDialog(
        title = "Review configuration",
        fade = FALSE,
        footer = bslib::layout_columns(
          col_widths = c(6,6), 
          shiny::actionButton(
            inputId = ns("save_review_config"),
            label = "Save",
            class = "btn-warning m2"
          ),
          modalButton("Cancel")
        ),
        shiny::checkboxGroupInput(
          ns("region_selection"), 
          label = "Select regions and sites to review",
          choices  = unique(sites$region), 
          selected = modvars$region_selection,
          inline = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = ns("site_selection"), 
          label = NULL, 
          choices = modvars$site_selection_choices, 
          selected = modvars$site_selection,
          options = shinyWidgets::pickerOptions(
            actionsBox = TRUE,
            size = 10,
            selectedTextFormat = "count > 3",
            countSelectedText = "{0} sites selected",
            style = "btn-outline-primary"
          ),
          multiple = TRUE
        ),
        htmltools::HTML("<hr><br>"),
        textOutput(ns("user_name")),
        selectizeInput(
          ns("active_role"), 
          label = "Active role:", 
          choices = r$user_roles(),
          selected = r$user_role()
        ),
        verbatimTextOutput(ns("review_config_feedback")),
        easyClose = TRUE
      )
    }
    
    output[["user_name"]] <- reactive({ paste0("Reviewer: ", r$user_name()) })
    
    observeEvent(input$config_review, showModal(review_modal()))
    
    observeEvent(input$region_selection, {
      selected_sites <- with(sites, site_code[region %in% input$region_selection])
      golem::cat_dev("update region selection to ", selected_sites, "\n")
      shinyWidgets::updatePickerInput(
        session = session,
        inputId  = "site_selection", 
        choices  = selected_sites,
        selected = selected_sites
      )
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$review_config_feedback <- renderText({
      req(!isTruthy(input$region_selection) | !isTruthy(input$site_selection))
      "You must select at least one site/region to review."
    })
    
    observeEvent(input$save_review_config, {
      golem::cat_dev("\nStart applying global configuration\n")
      req(input$site_selection)
      req(input$region_selection)
      
      if(!all(input$site_selection %in% sites_in_appdata)) return(
        warning("None of the selected sites are in the appdata. Filtering 
                aborted. To resolve this, verify sites provided to the module 
                with sites in the appdata ")
      )
      
      modvars$site_selection   <- input$site_selection
      
      golem::cat_dev("Selected sites:", modvars$site_selection, "\n")
      r <- filter_data(r, sites = input$site_selection, subject_ids = subject_ids, 
                       appdata = app_data, apptables = app_tables) |> 
        set_user_role(user_role = input$active_role)
      
      shiny::showModal(
        modalDialog(
          footer = modalButton("Close"), 
          "Review configuration applied successfully", 
          easyClose = TRUE, 
          fade = FALSE
        )
      )
    })
    
  })
}

