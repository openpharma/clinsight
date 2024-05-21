#' Queries - Shiny module UI
#'
#' @inherit mod_queries_server
#'
#' @seealso [mod_queries_server()], [mod_go_to_form_ui()], [mod_go_to_form_server()]
#' 
mod_queries_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(8,4),
      bslib::card(
        bslib::card_body(
          shinycssloaders::withSpinner(
            DT::DTOutput(ns("queries")),
            type = 5
          )
        ),
        bslib::card_body(
          max_height = "150px",
          mod_go_to_form_ui(ns("go_to_form")),
          fillable = FALSE
        )
      ),
      bslib::layout_columns(
        col_widths = c(12,12),
        bslib::card(
          id = ns("details_panel"),
          bslib::page_fluid(
            htmlOutput(ns("selected_query_title")),
            DT::DTOutput(ns("selected_query")),
            HTML("<br>")
          )
        ),
        mod_query_follow_up_ui(ns("query_follow_up_1"))
      )
    )
  )
}

#' Queries - Shiny module Server
#'
#' A Shiny module. Used to display all raised queries.
#'
#' The module displays all raised queries in a table. Upon clicking on a query,
#' the query follow-up messages will be shown. In addition, it will be possible
#' to write a follow-up message. The logic for writing the follow-up message is
#' extracted in a different module, named [mod_query_follow_up_server()]. New
#' queries are created in the module [mod_query_add_server()]. The button to
#' initiate this module can be found in the main sidebar
#' [mod_main_sidebar_server()].
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access the data frame `query_data`,
#'   and the reviewers' `user_name`. User name is needed for creating a
#'   follow-up to an existing query.
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @param db_path A character string with the path to the database.
#'
#' @inherit mod_go_to_form_server
#' @seealso [mod_queries_ui()], [mod_go_to_form_ui()],
#'   [mod_go_to_form_server()], [mod_query_follow_up_ui()],
#'   [mod_query_follow_up_server()], [mod_query_add_ui()],
#'   [mod_query_add_server()]. 
#' 
mod_queries_server <- function(id, r, navinfo, all_forms, db_path, table_names){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.character(db_path))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    selected_query <- reactive({
      req(nrow(initial_queries())>0)
      req(input$queries_rows_selected)
      with(initial_queries(), query_id[input$queries_rows_selected])
    })
    
    selected_query_data <- reactive({
      req(nrow(r$query_data)>0)
      req(selected_query())
      req(selected_query() %in% unique(r$query_data$query_id))
      with(r$query_data, r$query_data[query_id == selected_query(), ])
    })
    
    mod_query_follow_up_server("query_follow_up_1",  r = r, 
                               selected_query = selected_query, db_path = db_path)
    
    initial_queries <- reactive({
      df <- r$query_data |> 
        dplyr::filter(n == 1) 
      if(nrow(df) == 0) return(df) 
      df |>
        dplyr::slice_min(timestamp, by = c(subject_id, event_label, query_id)) |>
        dplyr::arrange(.data[["resolved"]])
    })
    
    mod_go_to_form_server(
      id = "go_to_form", 
      r = r, 
      navinfo = navinfo,
      navtable = initial_queries, 
      tablerow = reactive(input$queries_rows_selected),
      all_forms = all_forms,
      form_name = "item_group", 
      subject_id = "subject_id"
    )
    
    output[["queries"]] <- DT::renderDT({
      req(initial_queries())
      datatable_custom(
        initial_queries()[c("subject_id", "event_label", "item_group", "timestamp", "query", "resolved")], 
        table_names, 
        title = "All queries"
        )
    })
    
    output[["selected_query_title"]] <- renderText({
      req(selected_query_data())
      df <- selected_query_data()[1,]
      query_title <-  paste0(
        "<b><center>", 
        tags$h5(htmlEscape(df$subject_id)), 
        "<br>",
        htmlEscape(df$item), " (", 
        htmlEscape(df$item_group), "); ", 
        df$event_label, 
        "</center><br> resolved: ", 
        tags$i(df$resolved),
        "</b>",
        tags$line()
      )
    })
    
    output[["selected_query"]] <- DT::renderDT({
      req(selected_query_data())
      datatable_custom(
        selected_query_data()[c("reviewer", "timestamp", "query")], 
        table_names, 
        options = list(dom = 't', ordering = FALSE, pageLength = 100, scrollY = "200px"),
        class = "row-border hover",
        rownames = FALSE,
        selection = "none"
      )
    })
  })
}

## To be copied in the UI
# mod_queries_ui("queries_1")

## To be copied in the server
# mod_queries_server("queries_1")
