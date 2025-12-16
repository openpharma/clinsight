describe(
  "mod_header_widgets. Feature 1 | Load application module in isolation.", 
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_header_widgets_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_header_widgets_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        r = reactiveValues(
          filtered_data = list(
            "AEs" = data.frame(
              "subject_id" = "Subj01", 
              "event_name" = "Screening",
              "event_label" = "V0", 
              "item_name" = "Other"
            )
          )
        ), 
        rev_data = reactiveValues(), 
        navinfo = reactiveValues(),
        timeline_data = reactive(data.frame())
      ) 
      testServer(mod_header_widgets_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_header_widgets. Feature 2 | Get overview statistics of selected patient. 
      As a user, I want to be able to get an overview
      of the selected, active patient's data in the header. The information should 
      show the patient ID, the number of adverse events (including a color code 
      showing if new/updated adverse events are available), the number of forms that 
      need a review, and a timeline figure showing the number of visits that the 
      patient performed.", 
  {
    it(
      "Scenario 1 - Subject 1, with Adverse Events and a Serious Adverse Event. 
        Given data sets [rev_data] and [nav_info] and 
        [r$filtered_tables$`Adverse events`] set with test data, 
        and the active subject ID [r$subject_id] set to ['Subj01'] , 
        and the active subject having two adverse events and one severe adverse event,
        and the data frame [rev_data$summary()] containing data of 'Subj01' with 
        the column 'reviewed' containing at least one 'No',
        I expect [AEvals_active] to be a data frame with the adverse events of ['Subj01'],
        and [SAEvalue.individual] to be the value one,
        and [AEvalue.individual] to be the value two,
        and [all_AEs_reviewed] to be the value 'FALSE',
        and the output [ae_box] to contain a html element,
        and the ouput [visit_figure] to contain a plot object.", 
      {
        AE_table <- data.frame(
          "subject_id" = "Subj01", 
          "form_repeat" = 1:3, 
          `Serious Adverse Event` = c("No", "Yes", "No"), 
          check.names = FALSE
        )
        AE_figure_data <- data.frame(
          "subject_id" = "Subj01", 
          "event_name" = "Screening",
          "event_label" = factor("V0"), 
          "item_name" = "Other"
        )
        
        timeline_data <- data.frame()
        
        testargs <- list(
          r = reactiveValues(
            filtered_data = list("Adverse events" = AE_figure_data),
            filtered_tables = list("Adverse events" = AE_table)
          ), 
          rev_data = reactiveValues(
            summary = reactive({
              data.frame(
                "subject_id" = "Subj01",
                "Form" = "Adverse events",
                reviewed = c("No", "Yes")
              )
            })
          ), 
          navinfo = reactiveValues(),
          timeline_data = reactive(timeline_data)
        ) 
        
        testServer(mod_header_widgets_server, args = testargs, {
          ns <- session$ns
          r$subject_id = "Subj01"
          session$flushReact()
          expect_equal(AEvals_active(), AE_table)
          expect_equal(SAEvalue.individual(), 1)
          expect_equal(AEvalue.individual(), 2)
          expect_false(all_AEs_reviewed())
          expect_true(inherits(output$ae_box$html, "html"))
          expect_equal(output[["visit_figure"]]$alt, "Plot object")
        })
        
      }
    )
    it(
      "Scenario 2 - Subject 2, no AEs recorded. Given data sets [rev_data] and [nav_info] and 
        [r$filtered_data$`Adverse events`] set with test data, 
        and the active subject ID [r$subject_id] set to ['Subj02'], 
        and the active subject having no adverse event data available, 
        and the data frame [rev_data$summary()] containing no data of ['Subj02'],
        I expect SAEvalue.individual() to be zero,
        and AEvalue.individual() to be zero,
        and the AEvals_active() table to be a data frame with zero rows,
        and all_AEs_reviewed() to being set to 'TRUE',
        and output$ae_box to contain a html element,
        and ouput$visit_figure to contain a plot object.", 
      {
        AE_table <- data.frame(
          "subject_id" = "Subj01", 
          "form_repeat" = 1, 
          "Serious Adverse Event" = "No", 
          "event_name" = "Screening",
          "event_label" = factor("V0"), 
          "event_date" = as.Date("2025-12-16"),
          "start date" = "2025-12-16",
          "end date"   = "",
          "SAE Start date" = "",
          "SAE End date"  = "",
          "Name" = "Epistaxis",
          check.names = FALSE
        )
        AE_figure_data <- data.frame(
          "subject_id" = "Subj01", 
          "event_name" = "Screening",
          "event_label" = factor("V0"), 
          "event_date" = as.Date("2025-12-16"),
          "item_name" = "Other"
        )
        timeline_data <- get_timeline_data(list("Adverse events" = AE_figure_data), list("Adverse events" = AE_table))
        
        testargs <- list(
          r = reactiveValues(
            filtered_data = list("Adverse events" = AE_figure_data),
            filtered_tables = list("Adverse events" = AE_table)
          ), 
          rev_data = reactiveValues(
            summary = reactive({
              data.frame(
                "subject_id" = "Subj01",
                "Form" = "Adverse events",
                reviewed = c("No", "Yes")
              )
            })
          ), 
          navinfo = reactiveValues(),
          timeline_data = reactive(timeline_data)
        ) 
        
        testServer(mod_header_widgets_server, args = testargs, {
          ns <- session$ns
          r$subject_id = "Subj02"
          session$flushReact()
          expect_equal(AEvals_active(), AE_table[0,c("subject_id", "form_repeat", "Serious Adverse Event")])
          expect_equal(SAEvalue.individual(), 0)
          expect_equal(AEvalue.individual(), 0)
          expect_true(all_AEs_reviewed())
          expect_true(inherits(output$ae_box$html, "html"))
          expect_equal(output[["visit_figure"]]$alt, "Plot object")
          expect_true(inherits(output[["timeline_fig-timeline"]], "json"))
        })
        
      }
    )
  }
)

# describe(
#   "Feature 3 | View and toggle timeline.
#       As a user, I want to be able to view the interactive timeline and toggle it on and off.",
#   {
#     it(
#       "Scenario 1 - View and toggle timeline. Given test data,
#           I want to  be able to by default see the interactive timeline for
#           common forms, and not see it in study forms, but I want to be able
#           to toggle it on or off.",
#       {
#         AE_table <- data.frame(
#           "subject_id" = "Subj01", 
#           "form_repeat" = 1, 
#           "Serious Adverse Event" = "No", 
#           "event_name" = "Screening",
#           "event_label" = factor("V0"), 
#           "event_date" = as.Date("2025-12-16"),
#           "start date" = "2025-12-16",
#           "end date"   = "",
#           "SAE Start date" = "",
#           "SAE End date"  = "",
#           "Name" = "Epistaxis",
#           check.names = FALSE
#         )
#         AE_figure_data <- data.frame(
#           "subject_id" = "Subj01", 
#           "event_name" = "Screening",
#           "event_label" = factor("V0"), 
#           "event_date" = as.Date("2025-12-16"),
#           "item_name" = "Other"
#         )
#         timeline_data <- get_timeline_data(list("Adverse events" = AE_figure_data), list("Adverse events" = AE_table))
#         
#         test_ui <- function(request){
#           tagList(
#             golem_add_external_resources(),
#             shinyjs::useShinyjs(),
#             bslib::page_navbar(
#               header = mod_header_widgets_ui("header_widgets_1"),
#               bslib::nav_panel(
#                 title = "Common forms",
#                 bslib::card(
#                   shinyWidgets::switchInput(
#                     inputId = "sf_toggle_timeline",
#                     label = icon("timeline"),
#                     value = FALSE,
#                     inline = TRUE
#                   )
#                 )
#               ), 
#               bslib::nav_panel(
#                 title = "Study forms",
#                 bslib::card(
#                   shinyWidgets::switchInput(
#                     inputId = "sf_toggle_timeline",
#                     label = icon("timeline"),
#                     value = FALSE,
#                     inline = TRUE
#                   )
#                 )
#               )
#             )
#           )
#         }
# 
#         test_server <- function(input, output, session){
#           session$userData$review_type <- reactiveVal()
#           observeEvent(input$sf_toggle_timeline, {
#           })
#           mod_header_widgets_server(
#             id = "header_widgets_1",
#             r = reactiveValues(
#               filtered_data = list("Adverse events" = AE_figure_data),
#               filtered_tables = list("Adverse events" = AE_table)
#             ), 
#             rev_data = reactiveValues(
#               summary = reactive({
#                 data.frame(
#                   "subject_id" = "Subj01",
#                   "Form" = "Adverse events",
#                   reviewed = c("No", "Yes")
#                 )
#               })
#             ), 
#             navinfo = reactiveValues(),
#             timeline_data = reactive(timeline_data)
#           ) 
#         }
#         test_app <- shinyApp(test_ui, test_server)
#         browser()
#         app <- shinytest2::AppDriver$new(
#           app_dir = test_app,
#           name = "study_forms",
#           width = 1619,
#           height = 955
#         )
#         withr::defer(app$stop())
# 
#       }
#     )
#   }
# )


