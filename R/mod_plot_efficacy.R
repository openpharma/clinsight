#' plot_efficacy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_efficacy_ui <- function(id) {
  ns <- NS(id)
  # Create custom class to color navset_card_tab background to white
  div(
    class = "efficacy-card",
    tags$style(HTML(
      "
      .efficacy-card .card-header {
        background-color: #ffffff !important;
      }
      "
    )),
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "Waterfall Plot",
        plotly::plotlyOutput(ns("waterfall_plot"))
      ),
      bslib::nav_panel(
        title = "Spider Plot",
        plotly::plotlyOutput(ns("spider_plot"))
      ),
      full_screen = TRUE
    )
  )
}

#' plot_efficacy Server Functions
#'
#' @noRd
mod_plot_efficacy_server <- function(
  id,
  form,
  form_data,
  form_review_data,
  review_type = NULL,
  active_subject = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track selected patient (shared between both plots)
    selected_patient <- reactiveVal(NULL)

    observeEvent(
      {
        review_type()
        active_subject()
      },
      {
        rt <- review_type()
        as <- active_subject()

        if (!is.null(rt) && !is.null(as)) {
          if (rt == "subject") {
            # Only update if actually different to force invalidation
            if (!identical(selected_patient(), as)) {
              selected_patient(as)
            }
          } else if (rt == "form") {
            if (!is.null(selected_patient())) {
              selected_patient(NULL)
            }
          }
        }
      },
      priority = 100
    )

    data_p <- reactive({
      req(form_data())

      form_data() |>
        dplyr::mutate(
          # Clean HTML
          dplyr::across(
            .cols = where(is.character),
            .fns = \(x) clean_html(x)
          ),
          # Convert to numeric
          dplyr::across(
            .cols = c(
              `Day in Study`,
              Measurement,
              `Too Small to Measure`,
              `Not Evaluable`,
              `Sum of Target Lesion Diameters`,
              `Baseline - Sum of Target Lesion Diameters`,
              `% Change - TL SOD from Baseline`,
              `Nadir`,
              `TL SOD Change from Nadir`,
              `% Change - TL SOD from Nadir`,
              `Best % Change - Sum of Target Lesion Diameters`,
              `Best % Change - Sum of Target Lesion Diameters Imputed`
            ),
            .fns = \(x) as.numeric(x)
          )
        )
    })

    data_wf <- reactive({
      req(data_p())
      data_p() |>
        dplyr::select(
          subject_id,
          `Best % Change - Sum of Target Lesion Diameters`,
          `Best RECIST OR`
        ) |>
        dplyr::distinct() |>
        stats::na.omit()
    })

    data_s <- reactive({
      req(data_p())
      data_p() |>
        dplyr::select(
          subject_id,
          `Day in Study`,
          `% Change - TL SOD from Baseline`,
          `Best RECIST OR`
        ) |>
        dplyr::mutate(
          `Day in Study` = pmax(0, `Day in Study`, na.rm = TRUE)
        ) |>
        dplyr::distinct() |>
        stats::na.omit()
    })

    # Waterfall Plot
    output$waterfall_plot <- plotly::renderPlotly({
      req(data_wf())

      plot_data <- data_wf()
      selected <- selected_patient()

      # Modify opacity based on selection
      if (!is.null(selected)) {
        plot_data$plot_opacity <- ifelse(
          plot_data$subject_id == selected,
          1,
          0.2
        )
      } else {
        plot_data$plot_opacity <- 1
      }

      arcus.viz::plot_waterfall(
        data = plot_data,
        id = subject_id,
        response = `Best % Change - Sum of Target Lesion Diameters`,
        status = `Best RECIST OR`,
        static = FALSE,
        label_xaxis = "Unique Subject Identifier",
        label_yaxis = "Best % Change - Sum of Target Lesion Diameters",
        label_legend = "Best RECIST OR",
        textposition = "none",
        hovertext = rlang::expr(
          glue::glue(
            "
            {label_xaxis}: {subject_id}
            {label_yaxis}: {round(`Best % Change - Sum of Target Lesion Diameters`, 2)}%
            {label_legend}: {`Best RECIST OR`}
            "
          )
        ),
        colors = c(
          "Partial Response (PR)" = "#4DAF4A",
          "Progressive Disease (PD)" = "#000000",
          "Complete Response (CR)" = "#377EB8",
          "Stable Disease (SD)" = "#FFC72C"
        ),
        opacity = plot_data$plot_opacity,
        event_register = "plotly_click",
        source = "waterfall"
      ) |>
        plotly::layout(
          title = "Best Percentage Change - Sum of Target Lesion Diameters per Unique Subject Identifier"
        )
    })

    # Spider plot
    spider_data <- reactive({
      req(data_s())
      ds <- data_s() |>
        dplyr::mutate(
          subject_id = as.character(subject_id) # ensure character for matching
        )

      sel <- selected_patient()

      ds$plot_opacity <- if (!is.null(sel)) {
        ifelse(ds$subject_id == sel, 1, 0.2)
      } else {
        1
      }
      ds
    })
    output$spider_plot <- plotly::renderPlotly({
      req(data_s())
      sel <- selected_patient()
      ds <- data_s() |>
        dplyr::arrange(subject_id, `Day in Study`)

      arcus.viz::plot_spider(
        data = ds,
        id = subject_id,
        time = `Day in Study`,
        value = `% Change - TL SOD from Baseline`,
        color = `Best RECIST OR`,
        colors = c(
          "Partial Response (PR)" = "#4DAF4A",
          "Progressive Disease (PD)" = "#000000",
          "Complete Response (CR)" = "#377EB8",
          "Stable Disease (SD)" = "#FFC72C"
        ),
        marker_size = 14,
        highlight_id = sel,
        highlight_marker_size = 16,
        faded_opacity = 0.2,
        highlight_opacity = 1,
        trace_lines = TRUE,
        event_register = "plotly_click",
        source = "spider",
        ytitle = "% Change - TL SOD from Baseline"
      )
    })

    # Force complete re-render by changing output ID
    outputOptions(output, "spider_plot", suspendWhenHidden = TRUE)

    # Click handlers
    observeEvent(
      plotly::event_data("plotly_click", source = "waterfall"),
      {
        click_data <- plotly::event_data("plotly_click", source = "waterfall")
        if (!is.null(click_data)) {
          clicked_patient <- click_data$customdata
          golem::cat_dev("Waterfall click - clicked patient:", clicked_patient, "\n")

          if (identical(selected_patient(), clicked_patient)) {
            selected_patient(NULL)
          } else {
            selected_patient(clicked_patient)
          }
        }
      },
      priority = 50
    )

    observeEvent(
      plotly::event_data("plotly_click", source = "spider"),
      {
        click_data <- plotly::event_data("plotly_click", source = "spider")
        golem::cat_dev("Spider click event received!\n")
        # golem::cat_dev("Click data:\n")
        # print(click_data)

        if (!is.null(click_data) && !is.null(click_data$customdata)) {
          clicked_patient <- as.character(click_data$customdata[1]) # Get first element and convert to character
          golem::cat_dev("Spider click - clicked patient:", clicked_patient, "\n")

          # Only toggle if clicking the same patient that's already selected
          if (identical(selected_patient(), clicked_patient)) {
            golem::cat_dev("Deselecting patient\n")
            selected_patient(NULL)
          } else {
            golem::cat_dev("Selecting new patient:", clicked_patient, "\n")
            selected_patient(clicked_patient)
          }
        }
      },
      priority = 50,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Debug selected_patient changes
    observeEvent(
      selected_patient(),
      {
        golem::cat_dev("Selected patient is now:", selected_patient(), "\n")
      },
      ignoreInit = TRUE
    )
  })
}
