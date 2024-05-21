describe(
  "app_server(). Feature 1 | As a user, I want to be able to start the application", 
  {
    it("Can load the server without errors", {
      db_path <- withr::local_tempfile(fileext = ".sqlite")
      app_session <- MockShinySession$new()
      app_session$options$golem_options <- list(
        "meta" = metadata,
        "data" = clinsightful_data,
        "user_db" = db_path,
        "test_mode" = TRUE
      )
      testServer(app_server, {
        ns <- session$ns
        expect_true(grepl("test", ns("test")))
      },
      session = app_session) |> 
        suppressWarnings()
    })
    
    it("Can change main tabs by clicking on the tab, and will save 
       the result in an internal reactive value.", {
         db_path <- withr::local_tempfile(fileext = ".sqlite")

         app_session <- MockShinySession$new()
         app_session$options$golem_options <- list(
           "meta" = metadata,
           "data" = clinsightful_data,
           "user_db" = db_path,
           "test_mode" = TRUE
         )
         
         testServer(app_server, {
           ns <- session$ns
           session$setInputs(main_tabs = "Common events", common_data_tabs = "Adverse events")
           expect_equal(navinfo$active_tab, "Common events")
           expect_equal(navinfo$active_form, "Adverse events")
           
           session$setInputs(main_tabs = "Study data", study_data_tabs = "ECG")
           expect_equal(navinfo$active_tab, "Study data")
           expect_equal(navinfo$active_form, "ECG")
           session$setInputs(study_data_tabs = "Electrolytes")
           expect_equal(navinfo$active_tab, "Study data")
           expect_equal(navinfo$active_form, "Electrolytes")
         }, 
         session = app_session) |> 
           suppressWarnings()
       })
    
    # it("Scenario 1. Given [preconditions], and [inputs], I expect ...", {
    #   testServer(
    #     app_server(app_data = appdata, app_tables = apptables, 
    #                app_vars - vars, meta = metadata), {
    #       ns <- session$ns
    #       # session$setInputs(x = 1)
    #       # session$flushReact()
    #       # expect_true(input$x == 1)
    #       # - Testing output
    #       # expect_true(inherits(output$tbl$html, "html"))
    #     })
    # })
  }
)
