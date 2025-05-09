library(shinytest2)

describe(
  "Feature 5 | Save row level review. As a user, I want to be able to save a review of 
  only specific rows in a table. ", {
    
    app <- AppDriver$new(
      app_dir = test_path("fixtures/testapp"),
      name = "app-feature-5",
      timeout = 20000,
      width = 1619, 
      height = 955    
    )
    withr::defer(app$stop())
    # To count medication form checkboxes with specific characteristics with custom JS:
    count_med_checkboxes <- "$('#cf_medication-review_form_tbl-table input[type=\"checkbox\"]%s').length"
    it(
      "Scenario 1 - Save row review. 
            Given a fixed random test data set with all data marked as not yet reviewed, 
            and being logged in as test user, 
            and patient BEL_08_45 selected as active patient,
            and clicking on [Common events] to browse to the 'Medication' tab,
            and clicking on [Review Status] for rows 1 and 2,
            and clicking on [Save] to save the review,
            I expect that the data will be displayed as being partially reviewed,
            and that the selected data of the selected patient and form (Medication) 
            is marked as being reviewed with the reviewer name being 'test user'.", 
      {
        app$set_inputs(main_tabs = "Common events")
        app$wait_for_idle()
        app$set_inputs(common_data_tabs = "Medication")
        app$wait_for_idle()
        
        expect_equal(app$get_js(sprintf(count_med_checkboxes, "")), 7)
        
        app$run_js('$("#cf_medication-review_form_tbl-table input[type=\'checkbox\']").slice(0, 2).click()')
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":checked")), 2)
        
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle(800)
        output_names <- names(app$get_values(output = TRUE)$output)
        app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
        # Check that overall review state is partial
        expect_false(app$get_value(input = 'main_sidebar_1-review_forms_1-form_reviewed'))
        expect_true(app$get_js('$("#main_sidebar_1-review_forms_1-form_reviewed").prop("indeterminate")'))
        
        user_db <- app$get_value(export = "user_db")
        
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(
            subject_id == app$get_value(export = "active_participant"),
            item_group == app$get_value(export = "active_form")
          ) 
        reviewed_data <- active_form_data |> 
          dplyr::filter(form_repeat %in% 1:2)
        expect_equal(unique(reviewed_data$reviewed), "Yes")
        expect_equal(unique(reviewed_data$reviewer), "test user (Administrator)")
        not_review_data <- active_form_data |> 
          dplyr::filter(!form_repeat %in% 1:2)
        expect_equal(unique(not_review_data$reviewed), "No")
        expect_equal(unique(not_review_data$reviewer), "")
      }
    )
    it(
      "Scenario 2 - Partially undo row review. 
          Given a fixed random test data set 
            with the first two medications for subject BEL_08_45 in the 
            Medications form being reviewed and the rest not,
            and patient BEL_08_45 selected as active patient,
            and the [Medication] tab being the active form displayed,
            and clicking on [Review Status] for row 2,
            and clicking on [Save] to save the review,
            I expect that the review status will change,
            and that the selected data of the selected patient and form (Medication) 
            is marked as not yet being reviewed.", 
      {
        app$run_js('$("#cf_medication-review_form_tbl-table input[type=\'checkbox\']").slice(1, 2).click()')
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":checked")), 1)
        
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle(800)
        output_names <- names(app$get_values(output = TRUE)$output)
        app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
        expect_true(app$get_js('$("#main_sidebar_1-review_forms_1-form_reviewed").prop("indeterminate")'))
        
        user_db <- app$get_value(export = "user_db")
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(
            subject_id == app$get_value(export = "active_participant"),
            item_group == app$get_value(export = "active_form")
          )

        reviewed_data <- active_form_data |> 
          dplyr::filter(form_repeat == 1)
        expect_equal(unique(reviewed_data$reviewed), "Yes")
        expect_equal(unique(reviewed_data$reviewer), "test user (Administrator)")
        not_review_data <- active_form_data |> 
          dplyr::filter(form_repeat != 1)
        expect_equal(unique(not_review_data$reviewed), "No")
        expect_equal(unique(not_review_data$reviewer), c("test user (Administrator)", ""))
      }
    )
    it(
      "Scenario 3 - Showing all subjects in table during review. 
          Given a fixed random test data set 
            with some rows for subject BEL_08_45 in the 
            Medications form being reviewed and the rest not,
            and patient BEL_08_45 selected as active patient,
            and the [Medication] tab being the active form displayed,
            and clicking on [Show all participants],
            I expect that data of all patients will be shown in the table,
            and that only the previously reviewed rows of subject [BEL_08_45] 
            are marked as reviewed and the rest is not,
            and that, after clicking [Show all participants] again,
            the old state is restored.", 
      {
        app$run_js('$("#cf_medication-show_all_data").click()')
        app$wait_for_idle(800)
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":checked")), 1)
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":not(:checked)")), 89)

        #Only 7 medications of BEL_08_45 should have enabled checkboxes
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":is(:disabled)")), 83)
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":not(:disabled)")), 7)
        
        output_names <- names(app$get_values(output = TRUE)$output)
        ## snapshot 003:
        app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
        
        app$run_js('$("#cf_medication-show_all_data").click()')
        app$wait_for_idle(800)
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":checked")), 1)
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":not(:checked)")), 6)
      }
    )
    it(
      "Scenario 4 - Fully remove partial row review. 
          Given a fixed random test data set 
            with some rows for subject BEL_08_45 in the 
            Medications form being reviewed and the rest not,
            and patient BEL_08_45 selected as active patient,
            and the [Medication] tab being the active form displayed,
            and clicking on [Reviewed/form_review] twice in a row,
            and clicking on [Save] to save the review,
            I expect that, after the first click on [Reviewed], 
            all rows in the table are selected as reviewed,
            and after the second click on [Reviewed], all rows are deselected,
            and after clicking [Save],
            all rows are saved as not yet being reviewed.", 
      {
        app$run_js('$("#main_sidebar_1-review_forms_1-form_reviewed").click()')
        app$wait_for_idle(800)
        # All checkboxes should now be checked:
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":checked")), 7)
        
        all_values <- app$get_values(input = TRUE, output = TRUE)
        input_names <- vector_select(
          names(all_values$input),
          include = c(
            "cf_medication-review_form_tbl-table_review_selection"
          )
        )
        output_names <- vector_select(
          names(all_values$output),
          include = c(
            "cf_medication-review_form_tbl-table",
            "main_sidebar_1-navigate_forms_1-form_name",
            "main_sidebar_1-review_forms_1-form_reviewed",
            "main_sidebar_1-review_forms_1-progress_bar",
            "main_sidebar_1-review_forms_1-save_review_error"
          )
        )
        ## snapshot 004:
        app$expect_values(input = input_names, output = output_names)
        # somehow app$click doesn't register here, therefore using run_js:
        app$run_js('$("#main_sidebar_1-review_forms_1-form_reviewed").click()')
        app$wait_for_idle()
        # None of the checkboxes should now be checked:
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":checked")), 0)
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle(800)
        ## snapshot 005:
        app$expect_values(input = input_names, output = output_names)
        # Again, no checkboxes should be checked:
        expect_equal(app$get_js(sprintf(count_med_checkboxes, ":checked")), 0)
        expect_false(app$get_js('$("#main_sidebar_1-review_forms_1-form_reviewed").prop("indeterminate")'))
        expect_false(app$get_js('$("#main_sidebar_1-review_forms_1-form_reviewed").prop("checked")'))
        
        user_db <- app$get_value(export = "user_db")
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(
            subject_id == app$get_value(export = "active_participant"),
            item_group == app$get_value(export = "active_form")
          )
        expect_equal(unique(active_form_data$reviewed), "No")
        expect_equal(unique(active_form_data$reviewer), c("test user (Administrator)", ""))
      }
    )
  }
)
