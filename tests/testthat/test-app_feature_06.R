library(shinytest2)

describe(
  "Feature 6 | Save form level review. As a user, I want to be able to save a review of an entire form.", {
    app <- AppDriver$new(
      app_dir = test_path("fixtures/testapp"),
      name = "app-feature-6",
      timeout = 20000,
      width = 1619, 
      height = 955    
    )
    withr::defer(app$stop())
    it(
      "Scenario 1 - Save form-level review. 
            Given a fixed random test data set with all data marked as not yet reviewed, 
            and being logged in as test user, 
            and browsing to the 'Adverse events' tab of subject 'BEL_04_772',
            and clicking on [form_review],
            and clicking on [Save] to save the review,
            I expect that I see a modal asking to confirm the review of all data,
            and that, after confirming to save all,
            all data in the Adverse events tables is saved as being reviewed", 
      {
        app$wait_for_idle(8000)
        app$run_js('$("#start_page_1-overview_table td").filter(function() {return $(this).text() == "BEL_04_772"}).closest("tr").trigger("dblclick")')
        app$wait_for_idle()
        expect_equal(app$get_value(export = "active_form"), "Adverse events")
        expect_equal(app$get_value(export = "active_participant"), "BEL_04_772")
        expect_equal(
          app$get_js('$("#cf_adverse_events-review_form_tbl-table input[type=\'checkbox\']").length'),
          5
        )
        app$set_inputs("main_sidebar_1-review_forms_1-review_type" = "form")
        app$wait_for_idle(1000)
        expect_equal(
          app$get_js('$("#cf_adverse_events-review_form_tbl-table input[type=\'checkbox\']").length'),
          77
        )
        app$wait_for_idle()
        expect_equal(app$get_value(input = "cf_adverse_events-show_all_data"), TRUE)
        expect_equal(app$get_value(output = "form_level_review"), TRUE)
        
        app$set_inputs("main_sidebar_1-review_forms_1-form_reviewed" = TRUE)
        app$wait_for_idle()
        #All checkboxes should now be checked:
        expect_equal(
          app$get_js('$("#cf_adverse_events-review_form_tbl-table input[type=\'checkbox\']:checked").length'),
          77
        )
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle()
        # Warning/confirmation should show up:
        modal_text <- app$get_js('$("#main_sidebar_1-review_forms_1-confirm_save_modal").text()') |> 
          gsub(pattern = " *\n +", replacement = " ")
        expect_true(
          grepl(
            "This will change the review status of ALL items in the form Adverse events to reviewed",
            modal_text
          )
        )
        input_names <- c(
          "cf_adverse_events-review_form_tbl-table_review_selection",
          "cf_adverse_events-review_form_SAE_tbl-table_review_selection",
          "main_sidebar_1-review_forms_1-confirm_saving"
        )
        output_names <-c(
          "form_level_review",
          "main_sidebar_1-navigate_forms_1-form_name",
          "main_sidebar_1-review_forms_1-form_reviewed",
          "main_sidebar_1-review_forms_1-progress_bar",
          "main_sidebar_1-review_forms_1-save_review_error"
        )
        app$expect_values(input = input_names, output = output_names)
        app$click("main_sidebar_1-review_forms_1-confirm_saving")
        app$wait_for_idle()
        app$expect_values(input = input_names, output = output_names)
        user_db <- app$get_value(export = "user_db")
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(
            subject_id == app$get_value(export = "active_participant"),
            item_group == app$get_value(export = "active_form")
          )
        expect_equal(unique(active_form_data$reviewed), "Yes")
        expect_equal(unique(active_form_data$reviewer), c("test user (Administrator)"))
      }
    )
    it(
      "Scenario 2 - Undo all reviews in a form. 
            Given the state of scenario 1, 
            with all data in the 'Adverse events' form marked as reviewed, 
            and having form-level review active,
            and unchecking the [form_review] checkbox,
            and clicking on [Save] to save the removal of the review,
            I expect that I see a modal asking to confirm the removal of the 
            review of all form data,
            and that, after confirming to save all,
            all data in the Adverse events tables is saved as not being reviewed", 
      {
        app$run_js('$("#main_sidebar_1-review_forms_1-form_reviewed").click()')
        app$wait_for_idle()
        # No checkboxes should now be checked:
        expect_equal(
          app$get_js('$("#cf_adverse_events-review_form_tbl-table input[type=\'checkbox\']:checked").length'),
          0
        )
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle()
        modal_text <- app$get_js('$("#main_sidebar_1-review_forms_1-confirm_save_modal").text()') |> 
          gsub(pattern = " *\n +", replacement = " ")
        expect_true(
          grepl(
            "This will change the review status of ALL items in the form Adverse events to not reviewed",
            modal_text
          )
        )
        app$click("main_sidebar_1-review_forms_1-confirm_saving")
        app$wait_for_idle()
        input_names <- c(
          "cf_adverse_events-review_form_tbl-table_review_selection",
          "cf_adverse_events-review_form_SAE_tbl-table_review_selection",
          "main_sidebar_1-review_forms_1-confirm_saving"
        )
        output_names <-c(
          "form_level_review",
          "main_sidebar_1-navigate_forms_1-form_name",
          "main_sidebar_1-review_forms_1-form_reviewed",
          "main_sidebar_1-review_forms_1-progress_bar",
          "main_sidebar_1-review_forms_1-save_review_error"
        )
        app$expect_values(input = input_names, output = output_names)
      }
    )
  }
)
