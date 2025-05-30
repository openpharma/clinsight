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
    # To count checkboxes with specific characteristics with custom JS:
    count_AE_checkboxes <- "$('#cf_adverse_events-review_form_tbl-table input[type=\"checkbox\"]%s').length"
    count_SAE_checkboxes <- "$('#cf_adverse_events-review_form_SAE_tbl-table input[type=\"checkbox\"]%s').length"
    count_VS_checkboxes <- "$('#sf_vital_signs-review_form_tbl-table input[type=\"checkbox\"]%s').length"
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
        app$run_js('$("#navigate_review_1-review_df td").filter(function() {return $(this).text() == "Adverse events"}).closest("tr").trigger("dblclick")')
        app$wait_for_idle()
        expect_equal(app$get_value(export = "active_form"), "Adverse events")
        expect_equal(app$get_value(export = "active_participant"), "BEL_04_772")
        
        expect_equal(app$get_js(sprintf(count_SAE_checkboxes, "")), 0)
        expect_equal(app$get_js(sprintf(count_AE_checkboxes, "")), 5)

        app$set_inputs("main_sidebar_1-review_forms_1-review_type" = "form")
        app$wait_for_idle(1000)
        
        expect_equal(app$get_js(sprintf(count_SAE_checkboxes, "")), 13)
        expect_equal(app$get_js(sprintf(count_AE_checkboxes, "")), 77)
        app$wait_for_idle()
        expect_equal(app$get_value(input = "cf_adverse_events-show_all_data"), TRUE)
        expect_equal(app$get_value(output = "form_level_review"), TRUE)
        
        app$set_inputs("main_sidebar_1-review_forms_1-form_reviewed" = TRUE)
        app$wait_for_idle()
        #All checkboxes should now be checked:
        
        expect_equal(app$get_js(sprintf(count_SAE_checkboxes, ":checked")), 13)
        expect_equal(app$get_js(sprintf(count_AE_checkboxes, ":checked")), 77)
        
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
        #snapshot 001:
        app$expect_values(input = input_names, output = output_names)
        app$click("main_sidebar_1-review_forms_1-confirm_saving")
        app$wait_for_idle()
        #snapshot 002:
        app$expect_values(input = input_names, output = output_names)
        user_db <- app$get_value(export = "user_db")
        
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(item_group == app$get_value(export = "active_form"))
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
        expect_equal(app$get_js(sprintf(count_SAE_checkboxes, ":checked")), 0)
        expect_equal(app$get_js(sprintf(count_AE_checkboxes, ":checked")), 0)
        
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
        #snapshot 003:
        app$expect_values(input = input_names, output = output_names)
      }
    )
    it(
      "Scenario 3 - Revert to subject level review and review a subject. 
            Given the state of scenario 2, 
            and reverting to subject-level review,
            I expect that I now only see the data from subject BEL_04_772 again,
            and that when I click on [form_reviewed] and then save,
            no warning message is displayed, 
            and all Adverse Event form data of BEL_04_772 is saved correctly as being reviewed.", 
      {
        app$set_inputs("main_sidebar_1-review_forms_1-review_type" = "subject")
        app$wait_for_idle(1000)
        
        expect_equal(app$get_js(sprintf(count_SAE_checkboxes, "")), 0)
        expect_equal(app$get_js(sprintf(count_AE_checkboxes, "")), 5)
        expect_equal(app$get_js(sprintf(count_AE_checkboxes, ":not(:checked)")), 5)
        
        expect_equal(app$get_value(input = "cf_adverse_events-show_all_data"), FALSE)
        expect_equal(app$get_value(output = "form_level_review"), FALSE)
        
        app$set_inputs("main_sidebar_1-review_forms_1-form_reviewed" = TRUE)
        app$wait_for_idle()
        #All visible checkboxes should now be checked:
        expect_equal(app$get_js(sprintf(count_AE_checkboxes, ":checked")), 5)
        
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle()
        user_db <- app$get_value(export = "user_db")
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(item_group == app$get_value(export = "active_form"))
        
        expect_equal(
          unique(with(active_form_data, reviewed[subject_id == "BEL_04_772"])), 
          "Yes"
        )
        expect_equal(
          unique(with(active_form_data, reviewer[subject_id == "BEL_04_772"])),
          "test user (Administrator)"
        )
        # Other rows in the database of the Adverse Events remain not reviewed:
        expect_equal(
          unique(with(active_form_data, reviewed[subject_id != "BEL_04_772"])), 
          "No"
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
        #snapshot 004:
        app$expect_values(input = input_names, output = output_names)
      }
    )
    it(
      "Scenario 4 - Form level review of a study form. 
            Given the state of scenario 3, 
            and browsing to Vital signs
            and reverting to form-level review,
            and clicking on form_review,
            I expect that I now see all vital signs data,
            and that when I click on [form_reviewed] and then save,
            and confirm the save in a warning message,
            that all data from the vital signs form is saved correctly as being reviewed.", 
      {
        app$set_inputs(main_tabs = "Study data", common_data_tabs = "Vital signs")
        app$wait_for_idle(1000)
        app$set_inputs("main_sidebar_1-review_forms_1-review_type" = "form")
        app$wait_for_idle(1000)
        
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, "")), 68)
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, ":not(:checked)")), 68)
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, ":not(:disabled)")), 68)
        
        expect_equal(app$get_value(input = "sf_vital_signs-show_all"), TRUE)
        expect_equal(app$get_value(output = "form_level_review"), TRUE)
        
        app$set_inputs("main_sidebar_1-review_forms_1-form_reviewed" = TRUE)
        app$wait_for_idle()
        #All checkboxes should now be checked:
        
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, ":checked")), 68)
        
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle()
        # Warning/confirmation should show up:
        modal_text <- app$get_js('$("#main_sidebar_1-review_forms_1-confirm_save_modal").text()') |> 
          gsub(pattern = " *\n +", replacement = " ")
        expect_true(
          grepl(
            "This will change the review status of ALL items in the form Vital signs to reviewed",
            modal_text
          )
        )
        input_names <- c(
          "sf_vital_signs-review_form_tbl-table_review_selection",
          "main_sidebar_1-review_forms_1-confirm_saving"
        )
        output_names <-c(
          "form_level_review",
          "main_sidebar_1-navigate_forms_1-form_name",
          "main_sidebar_1-review_forms_1-form_reviewed",
          "main_sidebar_1-review_forms_1-progress_bar",
          "main_sidebar_1-review_forms_1-save_review_error"
        )
        #snapshot 005:
        app$expect_values(input = input_names, output = output_names)
        app$click("main_sidebar_1-review_forms_1-confirm_saving")
        app$wait_for_idle()
        #snapshot 006:
        app$expect_values(input = input_names, output = output_names)
        user_db <- app$get_value(export = "user_db")
        
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(item_group == app$get_value(export = "active_form"))
        expect_equal(unique(active_form_data$reviewed), "Yes")
        expect_equal(unique(active_form_data$reviewer), c("test user (Administrator)"))
      }
    )
    it(
      "Scenario 5 - Review of single rows during form level review.
            Given the state of scenario 4, 
            and deselecting the review status of first row,
            and saving the review actions, 
            I expect that no save confirmation will pop up,
            and that the review status of the first row is indeed undone.", 
      {
        # this indirectly deselects the Vital Signs data of BEL_04_133 of events
        # 'Screening' and 'Visit 1':
        app$run_js('$("#sf_vital_signs-review_form_tbl-table input[type=\'checkbox\']").slice(0, 1).click()')
        app$wait_for_idle()
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, ":not(:checked)")), 1)
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, ":checked")), 67)
        
        selection_table <- "sf_vital_signs-review_form_tbl-table_review_selection"
        pending_review_data <- app$get_values(input = selection_table)$input[[selection_table]]
        expect_equal(unique(pending_review_data$row_id), 1)
        
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle()
        # Warning/confirmation should NOT show up:
        expect_false(app$get_js("$('#shiny-modal').hasClass('show');"))
        expect_equal(
          app$get_js('$("#main_sidebar_1-review_forms_1-confirm_save_modal").text()'), 
          ""
        )
        input_names <- c(
          "sf_vital_signs-review_form_tbl-table_review_selection",
          "main_sidebar_1-review_forms_1-confirm_saving"
        )
        output_names <-c(
          "form_level_review",
          "main_sidebar_1-navigate_forms_1-form_name",
          "main_sidebar_1-review_forms_1-form_reviewed",
          "main_sidebar_1-review_forms_1-progress_bar",
          "main_sidebar_1-review_forms_1-save_review_error"
        )
        #snapshot 007:
        app$expect_values(input = input_names, output = output_names)
        user_db <- app$get_value(export = "user_db")
        not_reviewed_data <- db_get_table(user_db) |> 
          subset(
            id %in% pending_review_data$id, 
            select = c("subject_id", "event_name", "reviewer")
          ) |> 
          unique()
        expect_equal(not_reviewed_data$event_name, "Screening")
        expect_equal(unique(not_reviewed_data$subject_id), "BEL_04_133")
        expect_equal(unique(not_reviewed_data$reviewer), "test user (Administrator)")
      }
    ) 
    it(
      "Scenario 6 - Revert to subject level review.
            Given the state of scenario 5, 
            and that I revert to subject-level review,
            and that I deselect the rows of subject BEL_04_772 from Visit 2 and 3,
            I expect that after clicing `save`,
            no save confirmation will pop up,
            and that the review status of the respective two rows is indeed undone.", 
      {
        app$set_inputs("main_sidebar_1-review_forms_1-review_type" = "subject")
        app$set_inputs("sf_vital_signs-switch_view" = "table")
       
        app$wait_for_idle()
        # this indirectly deselects the Vital Signs data of BEL_04_772 of events
        # 'Visit 2' and 'Visit 3'. 
        #  Split up because only the last click is stored in the review_selection table:
        app$run_js('$("#sf_vital_signs-review_form_tbl-table input[type=\'checkbox\']").slice(2, 3).click()')
        app$wait_for_idle()
        selection_table <- "sf_vital_signs-review_form_tbl-table_review_selection"
        pending_review_data <- app$get_values(input = selection_table)$input[[selection_table]]
        expect_equal(unique(pending_review_data$row_id), 10)
        app$run_js('$("#sf_vital_signs-review_form_tbl-table input[type=\'checkbox\']").slice(3, 4).click()')
        app$wait_for_idle()
        pending_review_data <- dplyr::bind_rows(
          pending_review_data,
          app$get_values(input = selection_table)$input[[selection_table]]
        )
        expect_equal(unique(pending_review_data$row_id), 10:11)
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, ":not(:checked)")), 2)
        expect_equal(app$get_js(sprintf(count_VS_checkboxes, ":checked")), 5)
        
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle()
        # Warning/confirmation should NOT show up:
        expect_false(app$get_js("$('#shiny-modal').hasClass('show');"))
        expect_equal(
          app$get_js('$("#main_sidebar_1-review_forms_1-confirm_save_modal").text()'), 
          ""
        )
        user_db <- app$get_value(export = "user_db")
        not_reviewed_data <- db_get_table(user_db) |> 
          subset(
            id %in% pending_review_data$id, 
            select = c("subject_id", "event_name", "reviewer")
          ) |> 
          unique()
        expect_equal(not_reviewed_data$event_name, c("Visit 2", "Visit 3"))
        expect_equal(unique(not_reviewed_data$subject_id), "BEL_04_772")
        expect_equal(unique(not_reviewed_data$reviewer), "test user (Administrator)")
      }
    ) 
  }
)
