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
        output_names <- names(app$get_values(output = TRUE)$output) |> 
          vector_select(exclude = c("visit_figure", "start_page_1-overview_table"))
        app$expect_values(output = output_names)
        
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
      "Scenario 2 - Attempt to save review without a role that allows to review.
       Given a fixed random test data set with all data marked as not yet reviewed, 
            and being logged in as test user, 
            and changing my role to 'Data Manager',
            I expect to see a message in the sidebar that I am not allowed to review,
            and that the save review button and form_reviewed chek box are disabled.
      ",
      {
        app$click("main_sidebar_1-review_config_1-config_review")
        app$wait_for_idle()
        app$set_inputs("main_sidebar_1-review_config_1-active_role" = "Data Manager")
        app$click("main_sidebar_1-review_config_1-save_review_config")
        # Hide the modal showing confirmation of changing config:
        app$run_js("$('#shiny-modal').modal('hide');")
        
        review_error <- app$get_value(output = "main_sidebar_1-review_forms_1-save_review_error")$message
        
        expect_equal(
          review_error,
          "With your current role ('Data Manager') you cannot save a review."
        )
        
        expect_true(app$get_js("document.getElementById('main_sidebar_1-review_forms_1-save_review').disabled;"))
        expect_true(app$get_js("document.getElementById('main_sidebar_1-review_forms_1-form_reviewed').disabled;"))
      }
    )
  }
)
