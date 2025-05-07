library(shinytest2)

describe(
  "Feature 6 | Save form level review. As a user, I want to be able to save a review of an entire form.", {
    #chromote::local_chrome_version("134", binary = "chrome-headless-shell")
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
            and browsing to the adverse events tab of subject BEL_04_772,
            and changing the review type to 'form_review',
            and clicking on [form_review],
            and clicking on [Save] to save the review,
            I expect that I see a modal asking to confirm the review of all data.", 
      {
        #browser()
        app$wait_for_idle(8000)
        app$run_js('$("#start_page_1-overview_table td").filter(function() {return $(this).text() == "BEL_04_772"}).closest("tr").trigger("dblclick")')
        app$wait_for_idle()
        expect_equal(app$get_value(input = "main_tabs"), "Common events")
        expect_equal(
          app$get_js('$("#cf_adverse_events-review_form_tbl-table input[type=\'checkbox\']").length'),
          5
        )
        #app$get_values(input = TRUE)
        app$set_inputs("main_sidebar_1-review_forms_1-review_type" = "form")
        app$wait_for_idle(1000)
        expect_equal(
          app$get_js('$("#cf_adverse_events-review_form_tbl-table input[type=\'checkbox\']").length'),
          77
        )
        app$wait_for_idle()
        app$set_inputs("main_sidebar_1-review_forms_1-form_reviewed" = TRUE)
        app$click("main_sidebar_1-review_forms_1-save_review")
        app$wait_for_idle()
        
        output_names <- names(app$get_values(output = TRUE)$output)
        app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
      }
    )
  }
)
