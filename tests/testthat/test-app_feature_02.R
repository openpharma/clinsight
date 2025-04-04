library(shinytest2)

describe(
  "Feature 2 | Save review. As a user, I want to be able to save a review of a specific item 
  of a patient successfully, and store a comment with the review. ", {
    
    app <- AppDriver$new(
      app_dir = test_path("fixtures/testapp"),
      name = "app-feature-2",
      timeout = 20000,
      width = 1619, 
      height = 955    
    )
    withr::defer(app$stop())
    it(
      "Scenario 1 - Save review. 
            Given a fixed random test data set with all data marked as not yet reviewed, 
            and being logged in as test user, 
            and patient 45 selected as active patient,
            and the 'Vital signs' tab selected as the first tab in the [Study data] tabs,
            and clicking on [Study data] to browse to the 'Vital signs' tab,
            and clicking on [Reviewed],
            and adding a comment 'test comment' in the comment field,
            and clicking on [Save] to save the review,
            I expect that the data will be displayed as being reviewed,
            and that all data of the selected patient and form (Vital signs) 
            is marked as being reviewed with the reviewer name being 'test user',
            and that the comment 'test comment' is saved successfully.", 
      {
        app$set_inputs(main_tabs = "Study data")
        app$wait_for_idle()
        app$set_inputs(
          `main_sidebar_1-review_forms_1-form_reviewed` = TRUE,
          `main_sidebar_1-review_forms_1-add_comment` = TRUE,
          `main_sidebar_1-review_forms_1-review_comment` = "test comment"
        )
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle(800)
        output_names <- names(app$get_values(output = TRUE)$output)
        app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
        
        user_db <- app$get_value(export = "user_db")
        
        active_form_data <- db_get_table(user_db) |> 
          dplyr::filter(
            subject_id == app$get_value(export = "active_participant"),
            item_group == app$get_value(export = "active_form")
          ) 
        expect_equal(unique(active_form_data$reviewed), "Yes")
        expect_equal(unique(active_form_data$reviewer), "test user (Administrator)")
        expect_equal(unique(active_form_data$comment), "test comment")
      }
    )
    it(
      "Scenario 2 - Save row review. 
            Given a fixed random test data set with all data marked as not yet reviewed, 
            and being logged in as test user, 
            and patient 45 selected as active patient,
            and clicking on [Common events] to browse to the 'Medication' tab,
            and clicking on [Review Status] for rows 1 and 2,
            and clicking on [Save] to save the review,
            I expect that the data will be displayed as being partially reviewed,
            and that the selected data of the selected patient and form (Medication) 
            is marked as being reviewed with the reviewer name being 'test user'.", 
      {
        app$set_inputs(
          main_tabs = "Common events",
          common_data_tabs = "Medication"
        )
        app$wait_for_idle()
        expect_equal(
          app$get_js('$("#cf_medication-review_form_tbl-table input[type=\'checkbox\']").length'),
          7
        )
        app$run_js('$("#cf_medication-review_form_tbl-table input[type=\'checkbox\']").slice(0, 2).click()')
        expect_equal(
          app$get_js('$("#cf_medication-review_form_tbl-table input[type=\'checkbox\']:checked").length'),
          2
        )
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle(800)
        output_names <- names(app$get_values(output = TRUE)$output)
        app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
        # Check that overall review state is partial
        expect_true(app$get_value(input = 'main_sidebar_1-review_forms_1-form_reviewed'))
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
  }
)
