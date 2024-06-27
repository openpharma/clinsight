#' Database Synchronization information - Shiny module UI
#'
#' @description A shiny Module.
#'
#' @inherit mod_db_synch_info_server 
#' @seealso [mod_db_synch_info_server()]
#' 
mod_db_synch_info_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::card(
      htmlOutput(ns("db_synch_info")) 
    )
  )
}

#' Database Synchronization information - Shiny module Server
#'
#' A shiny module. Used to display database synchronization information/
#'
#' The module is currently initiated in the main sidebar. It shows the time at
#' which the database was synchronized last, and the time of the latest EDC data
#' entry (or data edit). In addition, a warning message will pop up if the
#' synchronization date is either unknown or older than one day.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param app_data List of data frames. Contains the application data, with data
#'   of each form stored in a data frame. Required to derive the latest EDC
#'   date.
#' @param db_path Character vector with the path to the app database. Required
#'   to retrieve the latest database synch date that is stored in the data frame
#'   "db_synch_time".
#' @param current_date Current date. Standard `Sys.Date()`. Can be useful to set
#'   for testing purposes.
#' @param show_synch_warning Logical. Whether to show a pop-up message with a
#'   warning if database synchronization did not happen on the current day. Will
#'   normally be shown if in the configuration the app is set to production.
#'
#' @seealso [mod_db_synch_info_ui()]
mod_db_synch_info_server <- function(
    id, 
    app_data, 
    db_path, 
    current_date = Sys.Date(),
    show_synch_warning = isTRUE(get_golem_config("app_prod"))
    ){
  stopifnot(is.list(app_data))
  stopifnot(is.character(db_path))
  stopifnot(is_date(current_date))
  stopifnot(is.logical(show_synch_warning))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    synch_time <- reactive({
      tryCatch({
        if(!file.exists(db_path)) dbt <- NULL else{
          dbt <- get_db_connection(db_path) |> 
            DBI::dbGetQuery("SELECT synch_time FROM `db_synch_time`") |> 
            dplyr::pull() 
        }
        if(is.null(dbt) || is.na(as.Date(dbt))){"Unknown"} else dbt
      }, 
      error = function(e){"Unknown"}
      )
    })
    
    edc_latest_date <- reactive({
      tryCatch({
        as.character(get_max_time(app_data, col_name = "edit_date_time"))
      },
      error = function(e){"Unknown"}
      )
    })
    
    # hack to be able to test the db_synch date:

    synch_warning <- reactive({
      req(synch_time(), show_synch_warning)
      if(synch_time() == "Unknown") return({
        paste0(
          "The latest database synchronization date is <b>Unknown</b>.<br>",
          "The most recent EDC data entry is <br>",
          htmlEscape(edc_latest_date()),
          "."
        )
      })
      if(current_date != as.Date(synch_time())) return({
        paste0(
        "Latest update was <b>",
        difftime(current_date, as.Date(synch_time()), units = "days"),
        " days ago.</b><br>(", 
        synch_time(),
        ")"
        )
      })
      ""
    })
    
    observeEvent(synch_warning(), {
      req(synch_warning())
      req(synch_warning() != "")
      showModal(
        modalDialog(
          title = "Warning",
          HTML(
            synch_warning(),
            "<br><br><i>If this is unexpected, please contact an administrator to perform a ", 
            "new synchronization with the EDC.</i>"
          ),
          footer = modalButton("Dismiss"),
          fade = FALSE
        )
      )
    })
    
    output[["db_synch_info"]] <- renderText({
      paste0(
        "EDC Sync date: ",
        "<br>",
        htmlEscape(synch_time()),
        "<br><br>", 
        "EDC latest data: ", 
        htmlEscape(edc_latest_date())
      )
    })
  })
}

## To be copied in the UI
# mod_db_synch_info_ui("db_synch_time_1")

## To be copied in the server
# mod_db_synch_info_server("db_synch_time_1")
