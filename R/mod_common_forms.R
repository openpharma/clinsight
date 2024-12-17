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

    common_form_data <- reactiveVal()
    SAE_data <- reactiveVal()
    reload_data <- reactiveVal(0)
    observe({
      df <- {
        shiny::validate(need(
          !is.null(r$filtered_data[[form]]),
          paste0("Warning: no data found in the database for the form '", form, "'.")
        ))
        dplyr::left_join(
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
          create_table(expected_columns = names(form_items)) |> 
          dplyr::mutate(o_reviewed = Map(\(x, y) append(x, list(row_id = y)), 
                                         o_reviewed, 
                                         dplyr::row_number()))
      }
      common_form_data({
        if(form == "Adverse events") {
          df |> 
            dplyr::filter(!grepl("Yes", `Serious Adverse Event`)
            ) |> 
            dplyr::select(-dplyr::starts_with("SAE"))
        } else {
          df
        }
      })
      if (form == "Adverse events")
        SAE_data({
          df |>
            dplyr::filter(grepl("Yes", `Serious Adverse Event`)) |>
            dplyr::select(dplyr::any_of(
              c("o_reviewed", "subject_id","form_repeat", "Name", "AESI",  "SAE Start date",
                "SAE End date", "CTCAE severity", "Treatment related",
                "Treatment action", "Other action", "SAE Category",
                "SAE Awareness date", "SAE Date of death", "SAE Death reason")
            )) |>
            adjust_colnames("^SAE ")
        })
    })
    
    mod_timeline_server("timeline_fig", r = r, form = form)
    
    if (form == "Adverse events") {
      output[["SAE_table"]] <- DT::renderDT({
        req(form == "Adverse events")
        datatable_custom(
          isolate(subset(SAE_data(), input$show_all_data | subject_id == r$subject_id)), 
          rename_vars = c("Review Status" = "o_reviewed", table_names), 
          rownames= FALSE,
          title = "Serious Adverse Events", 
          escape = FALSE,
          selection = "none",
          callback = checkbox_callback,
          options = list(
            columnDefs = list(
              list(
                targets = "o_reviewed",
                orderable = FALSE,
                render = checkbox_render
              ),
              list(
                targets = "subject_id",
                visible = isolate(input$show_all_data)
              )),
            rowCallback = row_callback
          ))
      })
      SAE_proxy <- DT::dataTableProxy("SAE_table")
    }
    
    output[["common_form_table"]] <- DT::renderDT({
      datatable_custom(
        isolate(subset(common_form_data(), input$show_all_data | subject_id == r$subject_id)), 
        rename_vars = c("Review Status" = "o_reviewed", table_names), 
        rownames= FALSE,
        title = form, 
        escape = FALSE,
        selection = "none",
        callback = checkbox_callback,
        options = list(
          columnDefs = list(
            list(
              targets = "o_reviewed",
              orderable = FALSE,
              render = checkbox_render
            ),
            list(
              targets = "subject_id",
              visible = isolate(input$show_all_data)
            )),
          rowCallback = row_callback
        ))
    })
    common_form_proxy <- DT::dataTableProxy('common_form_table')
    
    # Ensure that review data gets reset whenever the review data or subject
    # being viewed is changed.
    observe({
      reload_data(reload_data() + 1)
      session$userData$update_checkboxes[[form]] <- NULL
      session$userData$review_records[[form]] <- data.frame(id = integer(), reviewed = character())
    }) |> 
      bindEvent(r$subject_id, r$review_data,
                ignoreInit = TRUE)
    
    observeEvent(session$userData$update_checkboxes[[form]], {
      reload_data(reload_data() + 1)
      checked <- session$userData$update_checkboxes[[form]]
      
      df <- common_form_data() |> 
        dplyr::mutate(o_reviewed = dplyr::if_else(subject_id == r$subject_id, 
                                                  lapply(o_reviewed, modifyList, list(updated = checked)),
                                                  o_reviewed))
      common_form_data(df)
      
      req(form == "Adverse events")
      df <- SAE_data() |> 
        dplyr::mutate(o_reviewed = dplyr::if_else(subject_id == r$subject_id, 
                                                  lapply(o_reviewed, modifyList, list(updated = checked)),
                                                  o_reviewed))
      SAE_data(df)
    })
    
    lapply(c("common_form", if (form == "Adverse events") "SAE"), \(tbl) {
      # Reactive value mimics table name
      tbl_data <- sprintf("%s_data", tbl)
      # Get review selection input to observe
      review_selection <- sprintf("%s_table_review_selection", tbl)
      observeEvent(input[[review_selection]], {
        # Update review values for session's user data
        session$userData$update_checkboxes[[form]] <- NULL
        session$userData$review_records[[form]] <-
          dplyr::rows_upsert(
            session$userData$review_records[[form]],
            input[[review_selection]][c("id", "reviewed")],
            by = "id"
          ) |> 
          dplyr::filter(!is.na(reviewed)) |> 
          # Ensure that only the current subject is being reviewed
          dplyr::semi_join(
            subset(r$review_data, subject_id == r$subject_id & item_group == form),
            by = "id"
          ) |> 
          # Only update records where the review status is being changed
          dplyr::anti_join(
            subset(r$review_data, subject_id == r$subject_id & item_group == form),
            by = c("id", "reviewed")
          ) |> 
          dplyr::arrange(id)
        
        
        # Update the table's data reactive
        df <- do.call(tbl_data, list())
        
        update_row <- dplyr::distinct(input[[review_selection]], reviewed, row_id)
        row_ids <- df$o_reviewed |> lapply(\(x) x[["row_id"]]) |> unlist()
        df[row_ids == update_row$row_id, "o_reviewed"] <- list(list(
          modifyList(df[row_ids == update_row$row_id,]$o_reviewed[[1]], 
                     list(updated = switch(update_row$reviewed, "Yes" = TRUE, "No" = FALSE, NA)))
        ))
        do.call(tbl_data, list(df))
      })
    })
    
    # Any time the data in the form table is updated, "show all" is toggled,
    # or the subject being viewed is changed, the server data for the datatable
    # needs to be updated
    observe({
      req(!is.null(input$show_all_data))
      req(common_form_data())
      DT::dataTableAjax(common_form_proxy$session, 
                      subset(common_form_data(), input$show_all_data | subject_id == r$subject_id),
                      rownames = FALSE,
                      outputId = common_form_proxy$rawId)
      req(SAE_data())
      DT::dataTableAjax(SAE_proxy$session, 
                      subset(SAE_data(), input$show_all_data | subject_id == r$subject_id), 
                      rownames = FALSE,
                      outputId = SAE_proxy$rawId)
    })
    # Any time the review table is updated, "show all" is toggled, or the
    # subject being viewed is changed, the datatable should be reloaded to show
    # the new data
    observeEvent(reload_data(), {
      req(!is.null(input$show_all_data))
      req(common_form_data())
      DT::reloadData(common_form_proxy)
      req(SAE_data())
      DT::reloadData(SAE_proxy)
    }, ignoreInit = TRUE)
    
    observeEvent(r$subject_id, {
      req(common_form_data())
      reload_data(reload_data() + 1)
      df <- common_form_data() |> 
        dplyr::mutate(o_reviewed = Map(\(x, y) modifyList(x, list(updated = NULL, disabled = y)), o_reviewed, subject_id != r$subject_id))
      common_form_data(df)
      req(SAE_data())
      df <- SAE_data() |> 
        dplyr::mutate(o_reviewed = Map(\(x, y) modifyList(x, list(updated = NULL, disabled = y)), o_reviewed, subject_id != r$subject_id))
      SAE_data(df)
    })
    
    observeEvent(input$show_all_data, {
      req(common_form_data())
      reload_data(reload_data() + 1)
      index <- match("subject_id", colnames(common_form_data())) - 1
      if (input$show_all_data) {
        DT::showCols(common_form_proxy, index)
      } else {
        DT::hideCols(common_form_proxy, index)
      }
      req(SAE_data())
      index <- match("subject_id", colnames(SAE_data())) - 1
      if (input$show_all_data) {
        DT::showCols(SAE_proxy, index)
      } else {
        DT::hideCols(SAE_proxy, index)
      }
    })
    
  })
}

## To be copied in the UI
# mod_common_forms_ui("common_forms_1")

## To be copied in the server
# mod_common_forms_server("common_forms_1")
