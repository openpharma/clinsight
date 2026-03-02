#' Study forms - Shiny module UI
#' 
#' @inherit mod_common_forms_server
#'
#' @seealso [mod_study_forms_server()]
#' 
mod_study_forms_ui <- function(id, form, form_items){
  ns <- NS(id)
  bslib::nav_panel(
    title = form,
    bslib::card(
      full_screen = T, 
      bslib::layout_sidebar(
        conditionalPanel(
          condition = "input.switch_view === 'graph'",
          ns = NS(id),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("figure"), height = "100%"),
            type = 5
          )
        ),
        conditionalPanel(
          condition = "input.switch_view === 'table'",
          ns = NS(id),
          mod_review_form_tbl_ui(ns("review_form_tbl"))
        ),
        sidebar = bslib::sidebar(
          position = "right", 
          bg = "white",
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
            ), 
            bslib::input_switch(
              id = ns("show_all_participants"),
              label = span(icon("people-group"), "All subjects"),
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.show_all_participants === true",
              ns = NS(id),
              bslib::input_switch(
                id = ns("show_all_hover_labels"),
                label = span(icon("tags", class = "hover-switch-icon"), "All hover labels"), 
                value = FALSE
              )
            ),
            div(
              id = ns("transformation_graph_container"),
              shinyWidgets::radioGroupButtons(
                inputId = ns("transformation_graph"),
                label = "Transformation", 
                choices = c("None" = "none"),
                size = "sm"
              )
            ),
            bslib::popover(
              tags$a("Legend", tags$sup(icon("circle-info")), class =  "link"),
              bslib::card_body(img(src="www/figure_legend.png"))
            )
          ),
          conditionalPanel(
            condition = "input.switch_view === 'table'",
            ns = NS(id),
            bslib::input_switch(
              id = ns("show_all"),
              label = span(icon("people-group"), "All subjects")
            ),
            bslib::input_switch(
              id = ns("show_limits"), 
              label = span(icon("arrow-down-up-across-line", class = "limit-switch-icon"), "Lab limits")
            ),
            div(
              id = ns("transformation_table_container"),
              shinyWidgets::radioGroupButtons(
                inputId = ns("transformation_table"),
                label = "Transformation", 
                choices = c("None" = "none"),
                size = "sm"
              )
            ),
            bslib::input_switch(
              id = ns("enable_text_wrap"),
              label = span(tags$img(src="www/text-wrap.svg", class = "textwrap-switch-icon"), "Text wrap")
            ),
            bslib::card_body(
              HTML("<b>Bold*:</b> New/updated data"), 
              fillable = FALSE
            )
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
#' @param item_info A data frame containing the names of the study forms (in the
#'   column `item_group`), and the columns `item_scale` `use_unscaled_limits`,
#'   which are used to customize the way the figures are shown in the page.
#' @inheritParams mod_common_forms_server
#' 
#' @seealso [mod_study_forms_ui()], [mod_review_form_tbl_ui()],
#'   [mod_review_form_tbl_server()]
#' 
mod_study_forms_server <- function(
    id, 
    form,
    form_data,
    form_review_data,
    form_items, 
    active_subject,
    id_item = c("subject_id", "event_name", "item_group", 
                "form_repeat", "item_name"),
    table_names = NULL,
    item_info
){
  stopifnot(is.character(form), length(form) == 1)
  stopifnot(is.reactive(form_data), is.reactive(form_review_data))
  stopifnot(is.character(form_items))
  stopifnot(is.reactive(active_subject))
  stopifnot(is.character(id_item))
  stopifnot(is.null(table_names) || is.character(table_names))
  stopifnot(is.data.frame(item_info))
  
  names(form_items) <- names(form_items) %||% form_items
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    data_types <- isolate(unique(form_data()$item_type))
    all_continuous <- (!is.null(data_types) && all(data_types == "continuous") )
    
    observeEvent(input$switch_view, {
      if (!all_continuous) {
        shinyWidgets::updateRadioGroupButtons(inputId = "switch_view", selected = "table")
        shinyjs::disable("switch_view")
      }
    }, 
    once = TRUE
    )
    observeEvent(input$show_limits, {
      if (!all_continuous) shinyjs::hide("show_limits")
    }, 
    once = TRUE
    )
    
    observeEvent(session$userData$review_type(), {
      golem::cat_dev(form, "| Updating tables to show '", 
                     session$userData$review_type(), "' level data\n", sep = "")
      bslib::update_switch(
        session = session,
        id = "show_all",
        value = identical(session$userData$review_type(), "form")
      )
      shinyjs::toggleState(
        id = "show_all",
        condition = identical(session$userData$review_type(), "subject")
      )
      shinyWidgets::updateRadioGroupButtons(
        inputId = "switch_view",
        selected = if(
          !all_continuous || identical(session$userData$review_type(), "form")
        ) {
          "table" 
        } else {
          "graph"
        }
      )
    })
    
    observeEvent(input$show_all, {
      req(isTRUE(input$show_all))
      bslib::update_switch(
        session = session,
        id = "enable_text_wrap",
        value = FALSE
      )
    })
    
    fig_data <- reactive({
      req(isTRUE(all_continuous))
      validate(need(
        form_data(),
        paste0("Warning: no data found in the database for the form '", form, "'.")
      ))
      status_df <- form_review_data()[c(id_item, "edit_date_time", "status", "reviewed")] |> 
        dplyr::mutate(edit_date_time = as.POSIXct(edit_date_time, tz = "UTC"))
      form_data()[simplify_string(form_data()$item_name) %in% input$filter, ] |>
        dplyr::left_join(status_df, by = c(id_item, "edit_date_time")) |> 
        dplyr::mutate(item_name = factor(item_name, levels = names(form_items)))
    }) |> 
      debounce(1000)
    
    cols <- c("item_scale", "use_unscaled_limits")
    # Ensure no errors even if cols are missing, with FALSE as default:
    scaling_data <- lapply(add_missing_columns(item_info, cols)[1, cols], isTRUE)
    
    observeEvent(form_data(), {
      has_standardized <- any(!is.na(form_data()[["value_standardized"]]))
      has_scaled <- isTRUE(scaling_data$item_scale) && any(!is.na(form_data()[["value_scaled"]]))
      
      data_types_table <- c("None" = "none", if (has_standardized) c("Standardized" = "standardized"))
      data_types_graph <- c(data_types_table, if (has_scaled) c("Scaled" = "scaled"))
      
      if (length(data_types_table) == 1L) {
        removeUI(selector = paste0("#", ns("transformation_table_container")))
      } else {
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "transformation_table",
          choices = data_types_table
        )
      }
      
      if (length(data_types_graph) == 1L) {
        removeUI(selector = paste0("#", ns("transformation_graph_container")))
      } else {
        shinyWidgets::updateRadioGroupButtons(
          session = session,
          inputId = "transformation_graph",
          choices = data_types_graph,
          selected = if (has_scaled) "scaled" else "none"
        )
      }
    },
    once = TRUE
    )
    
    mod_review_form_tbl_server(
      "review_form_tbl", 
      form = form,
      form_data = form_data, 
      form_review_data = form_review_data, 
      active_subject = active_subject,
      form_items = form_items,
      transformation = reactive(input$transformation_table %||% "none"),
      show_all = reactive(isTRUE(input$show_all) | identical(session$userData$review_type(), "form")), 
      enable_text_wrap = reactive(isTRUE(input$enable_text_wrap)),
      show_limits = reactive(isTRUE(input$show_limits)),
      table_names = table_names,
      title = form
    )
    
    ############################### Outputs: ###################################
    dynamic_figure <- reactive({
      req(nrow(fig_data()) > 0, scaling_data)
      yval <- switch(
        input$transformation_graph %||% "none", 
        "scaled" = "value_scaled", 
        "none" = "item_value", 
        "standardized" = "value_standardized",
        "item_value"
      )
      validate(need(
        fig_data()[[yval]], 
        "No non-missing data available. Check table view or non-transformed data."
      ))
      plot_cnt <- length(unique(fig_data()$item_name))
      plot_height <- if (plot_cnt > 1) {
        ceiling(0.5 * plot_cnt * 150) + 200
      } else {
        500
      }
      plotly_figure(
        data = fig_data(),
        fig = "timeseries_fig",
        xval = "day",
        id = "subject_id",
        id_to_highlight = active_subject(), 
        point_size = "reviewed",
        height = plot_height,
        show_all_participants = isTRUE(input$show_all_participants),
        show_all_hover_labels = input$show_all_hover_labels,
        label = if (yval == "value_standardized") "label_standardized" else "text_label",
        yval = yval,
        use_unscaled_limits = scaling_data$use_unscaled_limits
      )
    })
    
    output[["figure"]] <- plotly::renderPlotly({
      dynamic_figure()
    })
    
    if(form %in% c("Vital signs", "Vitals adjusted")){
      shiny::exportTestValues(
        fig_data = tryCatch(fig_data(), error = function(e) e)
      )
    } 
  })
}

