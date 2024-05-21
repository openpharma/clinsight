library(shinytest2)

describe(
  paste0("Feature 4 | Load data from raw csv files. As a user, I want to be able ", 
  "to load data from the raw csv file output from the EDC system, and show ", 
  "this data in the application."), {
    it(
      paste0("Scenario 1 | Load raw data from Viedoc and display in application from one patient. ", 
      "Given raw CSV data exported from the EDC system from patient [IME-9600-002], ", 
      "with, at both Screening and Visit 2 the [Systolic blood pressure] being 99, ", 
      "[Diastolic Blood Pressure] 77, [Pulse] 77, [Resp] 9, and [Temperature] 37.5, ", 
      "and (only at screening) [Weight] 70kg, ", 
      "and that I browse to the 'Study data' tab",
      "I expect that I see the Vital signs page of patient 'IME-9600-002', ", 
      "and that I see a figure with the data displayed, ",
      "and that I see a table with the data displayed after clicking on the table view, ", 
      "and that the data for the figure and table in the app is the same as ", 
      "the raw data in the CSV files."), 
      {
        options("shiny.testmode" = TRUE)
        app <- AppDriver$new(
          app_dir = testthat::test_path("fixtures", "testapp-raw"),
          name = "app-feature-4",
          timeout = 12000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        app$set_inputs(main_tabs = "Study data")
        app$wait_for_idle()
        # Snapshot JSON table output and verify that vital signs are shown:
        app$expect_values(
          input = "sf_vital_signs-switch_view",
          output = c("sf_vital_signs-figure", 
                     "navigate_participants_1-subject_info", 
                     "main_sidebar_1-navigate_forms_1-form_name")
        )
        app$set_inputs("sf_vital_signs-switch_view" = "table")
        app$wait_for_idle()
        app$expect_values(
          input = "sf_vital_signs-switch_view",
          output = c("sf_vital_signs-figure", 
                     "navigate_participants_1-subject_info", 
                     "main_sidebar_1-navigate_forms_1-form_name")
          )
        
        # Get a snapshot of the raw data of the figure:
        fig_data <- app$get_value(export = "sf_vital_signs-fig_data") |> 
          dplyr::select(subject_id, event_name, event_date, item_group, item_name, item_value,  
                        item_unit, lower_lim, upper_lim) |> 
          dplyr::arrange(item_name)
        expect_snapshot(fig_data)
        # Get a snapshot of the raw data in the table:
        table_data  <- app$get_value(export = "sf_vital_signs-table_data")
        expect_snapshot(print(table_data, width = Inf))
      }
    )
  }
)
