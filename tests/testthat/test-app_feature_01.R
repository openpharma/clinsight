library(shinytest2)

describe(
  "Feature 1 | Explore data. As a user, I want to be able to explore data in both
    common forms and study-specific forms of multiple participants in the application,
    so that I can appropriately assess whether there is a risk for the patient's safety.",
  {
    it("Scenario 1 - Data visualization.
          Given a fixed random test data set,
          with all data being marked as 'new'/not yet reviewed,
          and being logged in as test user,
          and setting the subject id to 'BEL_04_772',
          I expect that I can see the start page,
          and that, by clicking on clicking on [common events],
          I can see the adverse event page of the selected patient with all 
          data shown in bold (indicating the need for review),
          and that, by clicking on [Study data],
          I can see the Vital signs page of the selected patient, 
          with all data shown in large dots (indicating the need for review),
          and that, after clicking on table view,
          the same vital signs data will be shown in a table, 
          with all data in bold (indicating the need for review).",
       {
         app <- AppDriver$new(
           app_dir = test_path("fixtures/testapp"),
           name = "app-feature-1",
           timeout = 25000,
           width = 1619,
           height = 955
         )
         withr::defer(app$stop())
         app$wait_for_idle(8000)
         # focus on the output, since input has some values that are 
         # inconsistently created by DT::datatable (for example, 
         # xxx_table_rows_current)
         # also, remove ggplot figure since it is recommended to remove these 
         # from snaps, since they cannot be serialized to JSON and produce a 
         # flaky hash https://rstudio.github.io/shinytest2/articles/robust.html
         app$run_js("$('#navigate_participants_1-subject_info').click()")
         app$wait_for_idle(1500)
         app$set_inputs("navigate_participants_1-participant_selection" = "BEL_04_772")
         app$wait_for_idle(1000)
         app$click("navigate_participants_1-subj_apply")
         app$wait_for_idle()
         
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         app$set_inputs(main_tabs = "Common events")
         app$wait_for_idle()
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         app$set_inputs(main_tabs = "Study data")
         app$wait_for_idle()
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
         
         app$set_inputs("sf_vital_signs-switch_view" = "table")
         output_names <- names(app$get_values(output = TRUE)$output)
         app$expect_values(output = vector_select(output_names, exclude = "visit_figure"))
       }
    )
  }
)


