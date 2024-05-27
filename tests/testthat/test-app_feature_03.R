library(shinytest2)

describe(
  "Feature 3 | Save query. As a user, I want to be able to save a query based 
  on a specific item of a patient sucessfully, and store the query in the app database", {
    
    app <- AppDriver$new(
      app_dir = test_path("fixtures/testapp"),
      name = "app-feature-3",
      timeout = 12000,
      width = 1619,
      height = 955
    )
    withr::defer(app$stop())
    it(
      "Scenario 1 | Save normal query. 
        Given a fixed fixed random test data set with all data marked as not yet reviewed, 
          and being logged in as test user, 
          and patient BEL_08_45 selected as active patient,
          and the 'Vital signs' tab selected as the first tab in the [Study data] tabs,
          and clicking on [Study data] to browse to the 'Vital signs' tab, 
          and clicking on [Create query], 
            I expect that I see a window to write a query,
            and after selecting the time point [Screening],
            and selecting the item [Weight],
            and adding a query text 'test query' in the query field,
            and clicking on [Add query] to save the query,
            I expect that a new query has been successfully added to the database,
            with the query text being 'test query', 
            and the user name 'test_query', 
            and the subject id BEL_08_45',
            and the form name 'Vital Signs, 
            and the item name 'Weight', 
            and the event label 'Screening, 
            and the reviewer name 'test user'", 
      {
        app$set_inputs(main_tabs = "Study data")
        app$wait_for_idle()
        app$click("main_sidebar_1-write_query-create_query")
        app$wait_for_idle()
        # Remove plotly_relayout from snapshot since it includes width which 
        # can be fragile to snapshot.
        input_names <- names(app$get_values(input = TRUE)$input)
        output_names <- names(app$get_values(output = TRUE)$output)
        app$expect_values(
          input = vector_select(input_names, exclude = "plotly_relayout"), 
          output = vector_select(output_names, exclude = "visit_figure")
          )
        app$set_inputs(
          "main_sidebar_1-write_query-query_select_item" = "Weight",
          "main_sidebar_1-write_query-query_text" = "test query"
          )
        app$click("main_sidebar_1-write_query-query_add_input")
        app$wait_for_idle()
        # expect values here might give problems because the date of the new 
        # query is shown and will always be new. Mocking of the timestamp value 
        # needed? Or maybe the test below is sufficient? 
        user_db <- app$get_value(export = "user_db")
        query_database_data <- collect_query_data(user_db) 
        
        expect_equal(query_database_data$query, "test query")
        expect_equal(query_database_data$subject_id, "BEL_08_45")
        expect_equal(query_database_data$item_group, "Vital signs")
        expect_equal(query_database_data$item, "Weight")
        expect_equal(query_database_data$event_label, "Screening")
        expect_equal(query_database_data$reviewer, "test user")
        # close modal, so that next scenario can start without it:
        app$wait_for_js("$('#shiny-modal').modal('hide');")
      }
    )
    it(
      "Scenario 2 | Browse to patient and save major query. 
        Given a fixed fixed random test data set with all data marked as not yet reviewed, 
          and being logged in as test user, 
          and browsing to patient BEL_04_772 with tab 'Adverse events',
          and clicking on [Create query], 
            I expect that I see a window to write a query,
            and after selecting the item [Tachycardia],
            and adding a query text 'Major test query' in the query field,
            and marking the query as 'Major',
            and clicking on [Add query] to save the query,
            I expect that a new query has been successfully added to the database,
            with the query text being 'Major test query', 
            and the subject id BEL_08_45',
            and the form name 'Adverse events', 
            and the item name 'Tachycardia', 
            and the query type being 'Major',
            and the event label 'Any event', 
            and the reviewer name 'test user'", 
      {
        # scenario 2 is not fully isolated from scenario 1; however, this setup 
        # is faster since only one shinytest2 app needs to be started. 
        app$set_inputs(main_tabs = "Common events")
        app$set_inputs(common_data_tabs = "Adverse events")
        app$wait_for_js("$('#navigate_participants_1-subject_info').click()")
        app$wait_for_idle()
        app$set_inputs("navigate_participants_1-participant_selection" = "BEL_04_772")
        app$click("navigate_participants_1-subj_apply")
        app$wait_for_idle()
        app$click("main_sidebar_1-write_query-create_query")
        app$wait_for_idle()
        app$set_inputs(
          "main_sidebar_1-write_query-query_select_item" = "Tachycardia",
          "main_sidebar_1-write_query-query_text" = "Major test query"
        )
        app$click("main_sidebar_1-write_query-query_major")
        
        # Remove plotly_relayout from snapshot since it includes width which 
        # can be fragile to snapshot.
        input_names <- names(app$get_values(input = TRUE)$input)
        output_names <- names(app$get_values(output = TRUE)$output)
        app$expect_values(
          input = vector_select(
            input_names, 
            exclude = c("plotly_relayout", "shinyjs-navigate_participants_1", 
                        "timeline_window")
          ), 
          output = vector_select(output_names, exclude = "visit_figure")
        )
        app$click("main_sidebar_1-write_query-query_add_input")
        app$wait_for_idle()

        user_db <- app$get_value(export = "user_db")
        query_database_data <- collect_query_data(user_db)
        query_database_data <- query_database_data[
          query_database_data$subject_id == "BEL_04_772", 
          ]
        
        expect_equal(query_database_data$subject_id, "BEL_04_772")
        expect_equal(query_database_data$item_group, "Adverse events")
        expect_equal(query_database_data$item, "Tachycardia")
        expect_equal(query_database_data$event_label, "Any visit")
        expect_equal(query_database_data$query, "Major test query")
        expect_equal(query_database_data$type, "Major")
        expect_equal(query_database_data$reviewer, "test user")
      }
    )
  }
)
