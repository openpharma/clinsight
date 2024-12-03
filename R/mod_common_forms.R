#' Common forms - Shiny module UI
#'
#' @inherit mod_common_forms_server
#' @seealso [mod_common_forms_server()]
#' 
mod_common_forms_ui <- function(id, form){
  ns <- NS(id)
  bslib::nav_panel(
    title = form, 
    mod_timeline_ui(ns("timeline_fig")),
    bslib::layout_sidebar(
      fillable = FALSE,
      if(form == "Adverse events"){
        DT::dataTableOutput(ns("SAE_table"))
      },
      DT::dataTableOutput(ns("common_form_table")), 
      sidebar = bslib::sidebar(
        bg = "white", 
        position = "right",
        shinyWidgets::materialSwitch(
          inputId = ns("show_all_data"),
          label = "Show all participants", 
          status = "primary",
          right = TRUE
        ),
        bslib::card_body(
          HTML("<b>Bold*:</b> New/updated data"), 
          fillable = FALSE
          )
      )
    )
  )
}

#' Common forms - Shiny module Server
#'
#' Shiny module. Used to display common form data in the dedicated tab.
#'
#' The module displays tables of common form data in a wide format. Included
#' common forms are currently: `Adverse events`, `Medical History`,
#' `Medication`, and `Conc. Procedures`. The applicable forms can be flexibly
#' changed in the metadata. The tables shown are overview tables in wide format,
#' similar to the ones in [mod_study_forms_server()]. When the common form
#' `Adverse events` is selected, the module will show an additional table with
#' Severe Adverse Events above the table with Adverse Events. In addition, it
#' will show a timeline by calling module
#' [mod_timeline_ui()]/[mod_timeline_server()]. The timeline shows study events
#' (such as drug administrations) and study visits together with Adverse Events,
#' so that temporal relationships between these events can be quickly revealed.
#' The `common forms` module is used in the main server to create all applicable
#' common form pages.
#'
#' @param id Character string, used to connect the module UI with the module
#'   Server.
#' @param r Common reactive values. Used to access the data frames
#'   `review_data`, `filtered_tables`, and the active `subject_id`.
#'   `review_data` will be used to determine which rows will be displayed in
#'   bold and, for the form Adverse events, which timeline data should be
#'   highlighted.
#' @param form A character string with the name of the form to display.
#' @param form_items A named character vector with the names of the expected
#'   variables defined in the applicable `form`. Used in the module Server to
#'   make sure that all expected columns are always created, even if some
#'   variables are implicitly missing (which might occur if there are not yet
#'   any values available for a specific variable). Also, implicitly missing
#'   variables might give errors if part of the script relies on the variables'
#'   presence. See also the parameter `expected_columns` in
#'   [create_table.default()].
#' @param id_item Character vector containing the column names of the columns
#'   that can uniquely identify one item/row.
#' @param table_names An optional character vector. If provided, will be used
#'   within [datatable_custom()], to improve the column names in the final
#'   interactive tables.
#'
#' @seealso [mod_common_forms_ui()], [mod_timeline_ui()],
#'   [mod_timeline_server()]
#' 
mod_common_forms_server <- function(
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
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_active <- reactive({
      req(!is.null(input$show_all_data))
      shiny::validate(need(
        !is.null(r$filtered_data[[form]]),
        paste0("Warning: no data found in the database for the form '", form, "'.")
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
      if(!input$show_all_data){ 
        df <-  with(df, df[subject_id == r$subject_id, ]) 
      }
      df
    })
    
    mod_timeline_server("timeline_fig", r = r, form = form)
    
    output[["SAE_table"]] <- DT::renderDT({
      req(form == "Adverse events")
      SAE_data <- data_active() |> 
        dplyr::filter(grepl("Yes", `Serious Adverse Event`)) |> 
        dplyr::select(dplyr::any_of(
          c("o_reviewed", "subject_id","form_repeat", "Name", "AESI",  "SAE Start date", 
            "SAE End date", "CTCAE severity", "Treatment related", 
            "Treatment action", "Other action", "SAE Category", 
            "SAE Awareness date", "SAE Date of death", "SAE Death reason")
        )) |> 
        adjust_colnames("^SAE ")
      if(!input$show_all_data) SAE_data$subject_id <- NULL
      datatable_custom(
        SAE_data, 
        rename_vars = table_names, 
        rownames= FALSE,
        title = "Serious Adverse Events", 
        escape = FALSE,
        selection = "none",
        callback = checkbox_callback,
        options = list(
          columnDefs = list(list(
            targets = 0,
            render = checkbox_render
          )),
          createdRow = checkbox_create_callback
        ))
    })
    
    observeEvent(data_active(), {
      session$userData$update_checkboxes[[form]] <- NULL
      session$userData$review_records[[form]] <- data.frame(id = integer(), reviewed = character())
    })
    
    observeEvent(input$common_form_table_review_selection, {
      session$userData$update_checkboxes[[form]] <- NULL
      
      session$userData$review_records[[form]] <-
        dplyr::rows_upsert(
          session$userData$review_records[[form]],
          input$common_form_table_review_selection,
          by = "id"
        ) |> 
        dplyr::filter(!is.na(reviewed)) |> 
        dplyr::anti_join(
          subset(r$review_data, item_group == form),
          by = c("id", "reviewed")
        ) |> 
        dplyr::arrange(id)
    })
    
    observeEvent(session$userData$update_checkboxes[[form]], {
      checked <- session$userData$update_checkboxes[[form]]
      
      update_cbs(ns("common_form_table"), checked)
      update_cbs(ns("SAE_table"), checked)
    })
    
    observeEvent(input$SAE_table_review_selection, {
      session$userData$update_checkboxes[[form]] <- NULL
      
      session$userData$review_records[[form]] <-
        dplyr::rows_upsert(
          session$userData$review_records[[form]],
          input$SAE_table_review_selection,
          by = "id"
        ) |> 
        dplyr::filter(!is.na(reviewed)) |> 
        dplyr::anti_join(
          subset(r$review_data, item_group == form),
          by = c("id", "reviewed")
        ) |> 
        dplyr::arrange(id)
    })
    
    output[["common_form_table"]] <- DT::renderDT({
      df <- data_active()
      if(form == "Adverse events") {
        df <- df |> 
          dplyr::filter(!grepl("Yes", `Serious Adverse Event`)
          ) |> 
          dplyr::select(-dplyr::starts_with("SAE"))
      }
      if(!input$show_all_data) df$subject_id <- NULL
      datatable_custom(
        df, 
        rename_vars = table_names, 
        rownames= FALSE,
        title = form, 
        escape = FALSE,
        selection = "none",
        callback = checkbox_callback,
        options = list(
          columnDefs = list(list(
            targets = 0,
            render = checkbox_render
          )),
          createdRow = checkbox_create_callback
        ))
    })
    
  })
}

## To be copied in the UI
# mod_common_forms_ui("common_forms_1")

## To be copied in the server
# mod_common_forms_server("common_forms_1")
