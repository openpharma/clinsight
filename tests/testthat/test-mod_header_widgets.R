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
        timeline_data = data.frame(),
        available_data = data.frame()
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
        AE_figure_data <- data.frame(
          "subject_id" = "Subj01", 
          "event_name" = "Screening",
          "event_label" = "SCR",
          "item_group" = "Adverse events",
          "form_repeat" = 1:3,
          "item_name" = c(rep("Serious Adverse Event", times = 3), rep("Name", times = 3)), 
          "item_value" = c("Yes", "No", "No", "Sepsis", "Epistaxis", "UTI")
        )
        available_data <- get_available_data(list("Adverse events" = AE_figure_data))

        timeline_data <- data.frame()
        
        testargs <- list(
          r = reactiveValues(
            filtered_data = list("Adverse events" = AE_figure_data)
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
          timeline_data = timeline_data,
          available_data = available_data
        ) 
        
        testServer(mod_header_widgets_server, args = testargs, {
          ns <- session$ns
          r$subject_id = "Subj01"
          session$flushReact()
          expect_equal(
            all_aes(), 
            data.frame("subject_id" = "Subj01", AEs = 2, SAEs = 1)
          )
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
        I expect that zero AEs and zero SAEs are found for Subj02 in [all_aes()],
        and all_AEs_reviewed() to being set to 'TRUE',
        and output$ae_box to contain a html element,
        and ouput$visit_figure to contain a plot object.", 
      {
        AE_figure_data <- data.frame(
          "subject_id" = "Subj01", 
          "event_name" = "Screening",
          "event_label" = factor("V0"), 
          "event_date" = as.Date("2025-12-16"),
          "item_group" = "Adverse events",
          "form_repeat" = 1:3,
          "item_name" = c(rep("Serious Adverse Event", times = 3), rep("Name", times = 3)), 
          "item_value" = c("Yes", "No", "No", "Sepsis", "Epistaxis", "UTI")
        )
        
       vs_data <- data.frame(
          subject_id = c("Subj02"),
          item_name = c("other_event"),
          form_repeat = 1,
          item_group = c("vital_signs"),
          event_name = "",
          event_label = factor("V0")
        )
       
        # Class must be added since AE table is now created in get_timeline_data().
        class(AE_figure_data) <- c("adverse_events", class(AE_figure_data))
        available_data <- get_available_data(list("Adverse events" = AE_figure_data, "vs_data" = vs_data))
        
        timeline_data <- get_timeline_data(
          list("Adverse events" = AE_figure_data), 
          available_data = available_data
        )
        testargs <- list(
          r = reactiveValues(
            filtered_data = list("Adverse events" = AE_figure_data)
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
          timeline_data = timeline_data,
          available_data = available_data
        ) 
        
        testServer(mod_header_widgets_server, args = testargs, {
          ns <- session$ns
          r$subject_id = "Subj02"
          session$flushReact()
          expect_equal(
            dplyr::filter(all_aes(), subject_id == "Subj02"), 
            data.frame("subject_id" = "Subj02", AEs = 0, SAEs = 0)
          )
          expect_true(all_AEs_reviewed())
          expect_true(inherits(output$ae_box$html, "html"))
          expect_equal(output[["visit_figure"]]$alt, "Plot object")
          expect_true(inherits(output[["timeline_fig-timeline"]], "json"))
        })
        
      }
    )
  }
)

describe(
  "Feature 3 | View and toggle timeline.
      As a user, I want to be able to view the interactive timeline and toggle it on and off.",
  {
    it(
      "Scenario 1 - View and toggle timeline. Given small test data frame,
          I expect that the timeline is by default shown in common forms, 
          and by default not shown in study forms, 
          and that it can be toggled on of off in both study forms and common forms.",
      {
        AE_figure_data <- data.frame(
          "subject_id" = "Subj01", 
          "event_name" = "Screening",
          "event_label" = factor("V0"), 
          "event_date" = as.Date("2025-12-16"),
          "item_group" = "Adverse events",
          "form_repeat" = 1:3,
          "item_name" = c(rep("Serious Adverse Event", times = 3), rep("Name", times = 3)), 
          "item_value" = c("Yes", "No", "No", "Sepsis", "Epistaxis", "UTI")
        )
        
        vs_data <- data.frame(
          subject_id = c("Subj02"),
          item_name = c("other_event"),
          form_repeat = 1,
          item_group = c("vital_signs"),
          event_name = "",
          event_label = factor("V0")
        )
        
        # Class must be added since AE table is now created in get_timeline_data().
        class(AE_figure_data) <- c("adverse_events", class(AE_figure_data))
        
        appdata <- list("Adverse events" = AE_figure_data, "vs_data" = vs_data)
        available_data <- get_available_data(appdata)
        
        timeline_data <- get_timeline_data(
          list("Adverse events" = AE_figure_data), 
          available_data = available_data
        )
        test_ui <- function(request){
          tagList(
            golem_add_external_resources(),
            shinyjs::useShinyjs(),
            bslib::page_navbar(
              id = "main_tabs",
              header = mod_header_widgets_ui("header_widgets_1"),
              bslib::nav_panel(
                title = "Common events",
                bslib::card(
                  bslib::input_switch(id = "cf_toggle_timeline", "cf", value = TRUE),
                  min_height = "600px"
                )
              ),
              bslib::nav_panel(
                title = "Study data",
                bslib::card(
                  bslib::input_switch(id = "sf_toggle_timeline", "sf", value = FALSE),
                  min_height = "600px"
                )
              )
            )
          )
        }

        test_server <- function(input, output, session){
          session$userData$review_type <- reactiveVal()
          
          navinfo <- reactiveValues(
            active_form       = "Adverse events",
            active_tab        = "Common events",
            cf_toggle_timeline = reactive(input$cf_toggle_timeline),
            sf_toggle_timeline = reactive(input$sf_toggle_timeline)
          )
          observeEvent(input$main_tabs, {
            req(input$main_tabs != navinfo$active_tab)
            navinfo$active_tab <- input$main_tabs
          })
          
          mod_header_widgets_server(
            id = "header_widgets_1",
            r = reactiveValues(
              filtered_data = appdata,
              subject_id = "Subj01",
              filtered_subjects = c("Subj01", "Subj02")
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
            navinfo = navinfo,
            timeline_data = timeline_data,
            available_data = available_data
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "header_widgets",
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        expect_true(inherits(app$get_value(output = "header_widgets_1-timeline_fig-timeline"), "json"))
        
        timeline_visibile <- "document.getElementById('header_widgets_1-timeline_fig-timeline').checkVisibility();"
        expect_true(app$get_js(timeline_visibile))
        
        app$set_inputs("main_tabs" = "Study data")
        expect_false(app$get_js(timeline_visibile))
        
        app$run_js('$("#sf_toggle_timeline").click()')
        app$wait_for_idle()
        expect_true(app$get_js(timeline_visibile))
        
        app$set_inputs("main_tabs" = "Common events")
        expect_true(app$get_js(timeline_visibile))
        
        app$run_js('$("#cf_toggle_timeline").click()')
        app$wait_for_idle()
        expect_false(app$get_js(timeline_visibile))
      }
    )
  }
)


