#' Study forms - Shiny module UI
#' 
#' @inherit mod_study_forms_server
#'
#' @seealso [mod_study_forms_server()]
#' 
mod_study_forms_ui <- function(id, form, form_items){
  ns <- NS(id)
  bslib::nav_panel(
    title = form,
    bslib::card(
      full_screen = T,
      bslib::layout_columns(
        col_widths = c(2, -4, 2, -4, 12, 12),
        shinyWidgets::radioGroupButtons(
          inputId = ns("switch_view"),
          choiceNames = list(icon("line-chart"), icon("table-list")),
          choiceValues = list("graph", "table"),
          selected = "graph"
        ),
        conditionalPanel(
          condition = "input.switch_view === 'graph'",
          ns = NS(id),
          shinyWidgets::pickerInput(
            inputId = ns("filter"),
            label = NULL,
            choices = form_items,
            selected = form_items,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 3",
              style = "btn-outline-primary"
            ),
            multiple = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.switch_view === 'graph'",
          ns = NS(id),
          bslib::layout_columns(
            col_widths = c(8,4),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("figure"), height = "100%"),
              type = 5
            ),
            img(src="www/figure_legend.png", width = 200, height = 233)
          )
        ),
        bslib::card_body(
          conditionalPanel(
            condition = "input.switch_view === 'table'",
            ns = NS(id),
            shinyWidgets::materialSwitch(
              inputId = ns("show_all"),
              label = "Show all participants", 
              status = "primary",
              right = TRUE
            ),
            DT::dataTableOutput(ns("table"), width = "auto")
          )
        )
      )
    )
  )
}

#' Study forms - Shiny module Server
#'
#' A shiny module. Used to display figures and tables of a study-specific form
#' and will be accessible in through the study form tab on the main page.
#'
#' The module displays tables and figures, and shows data of the currently
#' active subject. In the figures, the data can easily be compared with data of
#' other study participants. Data that is new or updated will have large points,
#' so that the reviewer can focus on new data. By default, all items of a form
#' will be shown, but the reviewer can select single or multiple variables to
#' focus on in the figure. There is also an option to switch from graphical view
#' to table view. In a table view, the same data will be shown in wide-table
#' format, with new/updated data shown in bold. If the values contain units, the
#' original units will be shown. The `Study forms` module is used in the main
#' `app_server` to create all applicable study form pages.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access the data frames
#'   `review_data`, `filtered_data`, and the active `subject_id`. The latter is
#'   used to show data in the tables of the active subject id, and to highlight
#'   data in the figures. The data frame filtered data should contain a column
#'   "item_type". If all values in "item_type" are "continuous", time-series
#'   figures will be shown.
#' @param form A character string with the name of the form to display.
#' @param form_items A named character vector with the names of the expected
#'   variables defined in the applicable `form`. Used in the UI to create a
#'   filter with drop-down menu, to select the desired variables in a figure.
#'   Used in the module Server to make sure that all expected columns are always
#'   created, even if some variables are implicitly missing (which might occur
#'   if there are not yet any values available for a specific variable). Also,
#'   implicitly missing variables might give errors if part of the script relies
#'   on the variables' presence. See also the parameter `expected_columns` in
#'   [create_table.default()].
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#' @param id_item Character vector containing the column names of the columns
#'   that can uniquely identify one item/row.
#'
#' @seealso [mod_study_forms_ui()]
#' 
mod_study_forms_server <- function(
    id, 
    r, 
    form,
    form_items, 
    id_item = c("subject_id", "event_name", "item_group", 
                "form_repeat", "item_name"),
    table_names = NULL
){
  stopifnot(is.reactivevalues(r))
  stopifnot(is.character(form), length(form) == 1)
  stopifnot(is.character(form_items))
  stopifnot(is.character(id_item))
  names(form_items) <- names(form_items) %||% form_items
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    data_types <- isolate(unique(r$filtered_data[[form]]$item_type))
    all_continuous <- (!is.null(data_types) && all(data_types == "continuous") )
    if(!all_continuous){
      shinyWidgets::updateRadioGroupButtons(
        inputId = "switch_view",
        selected = "table"
      )
      shinyjs::disable("switch_view")
    }
    
    fig_data <- reactive({
      req(isTRUE(all_continuous))
      validate(need(
        r$filtered_data[[form]],
        paste0("Warning: no data found in the database for the form '", form, "'.")
      ))
      df <- r$filtered_data[[form]] 
      if(is.null(df)) return(NULL)
      status_df <- r$review_data |> 
        dplyr::filter(item_group == form) |> 
        dplyr::select(dplyr::all_of(c(id_item, "edit_date_time", "status", "reviewed"))) |> 
        dplyr::mutate(edit_date_time = as.POSIXct(edit_date_time, tz = "UTC"))
      df[simplify_string(df$item_name) %in% input$filter, ] |>
        dplyr::left_join(status_df, by = c(id_item, "edit_date_time")) |> 
        dplyr::mutate(item_name = factor(item_name, levels = names(form_items)))
    })
    
    table_data_active <- reactive({
      validate(need(
        r$filtered_data[[form]],
        paste0("Warning: no data found in database for the form '", form, "'")
      ))
      df <- dplyr::left_join(
        r$filtered_data[[form]],
        with(r$review_data, r$review_data[item_group == form, ]) |> 
          dplyr::select(-dplyr::all_of(c("edit_date_time", "event_date"))), 
        by = id_item
      ) |> 
        dplyr::mutate(
          item_value = ifelse(
            reviewed == "No", 
            paste0("<b>", htmltools::htmlEscape(item_value), "*</b>"), 
            htmltools::htmlEscape(item_value)
          )
        ) |> 
        create_table(expected_columns = names(form_items))
      req(nrow(df) != 0)
      if(input$show_all) return(df) 
      with(df, df[subject_id == r$subject_id, ]) |> 
        dplyr::select(-dplyr::all_of("subject_id"))
    })
    
    ############################### Outputs: ###################################
    dynamic_figure <- reactive({
      req(nrow(fig_data()) > 0)
      scale_yval <- as.logical(with(metadata$groups, item_scale[item_group == form]))
      yval <- ifelse(scale_yval, "value_scaled", "item_value")
      validate(need(
        fig_data()[[yval]], 
        ifelse(scale_yval, 
               "No non-missing scaled data available. Check table view.", 
               "No non-missing data available.")
      ))
      plotly_figure(
        data = fig_data(),
        fig = "timeseries_fig",
        xval = "day",
        id = "subject_id",
        id_to_highlight = r$subject_id, 
        point_size = "reviewed",
        height = ceiling(0.5*length(unique(fig_data()$item_name))*125+150),
        scale = as.logical(with(metadata$groups, item_scale[item_group == form])),
        use_unscaled_limits = as.logical(
          with(metadata$groups, use_unscaled_limits[item_group == form])
        )
      )
    })
    
    output[["figure"]] <- plotly::renderPlotly({
      dynamic_figure()
    })
    
    output[["table"]] <- DT::renderDT({
      req(table_data_active())
      datatable_custom(table_data_active(), table_names, escape = FALSE)
    })
    
    if(form == "Vital signs"){
      shiny::exportTestValues(
        table_data = table_data_active(),
        fig_data = fig_data()
      )
    } 
  })
}

## To be copied in the UI
# mod_study_forms_ui("study_form_element_1")

## To be copied in the server
# mod_study_forms_server("study_form_element_1")
