describe(
  "app_server(). Feature 1 | As a user, I want to be able to start the application", 
  {
    it("Can load the server without errors", {
      withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
      db_path <- withr::local_tempfile(fileext = ".sqlite")
      file.copy(testthat::test_path("fixtures/testdb.sqlite"), db_path)
      app_session <- MockShinySession$new()
      app_session$options$golem_options <- list(
        "meta" = metadata,
        "data" = clinsightful_data,
        "user_db" = db_path
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
         withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
         db_path <- withr::local_tempfile(fileext = ".sqlite")
         file.copy(testthat::test_path("fixtures/testdb.sqlite"), db_path)

         app_session <- MockShinySession$new()
         app_session$options$golem_options <- list(
           "meta" = metadata,
           "data" = clinsightful_data,
           "user_db" = db_path
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
    
    # it("Scenario 1 - Given [preconditions], and [inputs], I expect ...", {
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

describe(
  "app_server(). Feature 2 | As a user, I want to be warned if no user 
         role or user name is available", 
  {
    it("warns if there is no valid user role", {
      withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
      db_path <- withr::local_tempfile(fileext = ".sqlite")
      file.copy(testthat::test_path("fixtures/testdb.sqlite"), db_path)
      app_session <- MockShinySession$new()
      app_session$options$golem_options <- list(
        "meta" = metadata,
        "data" = clinsightful_data,
        "user_db" = db_path
      )
      local_mocked_bindings(
        authenticate_server = function(...) {
          reactiveValues(
            user = "test_user",
            name = "Test user",
            roles = "",
            sites = data.frame("site_code" = "Site 01", "region" = "DEU")
          )
        }
      )
      
      testServer(app_server, {
        ns <- session$ns
        session$flushReact()
        expect_equal(
          user_error(),
          paste0("No valid user role provided. Functionality is limited. ",
                 "Please contact the administrator to resolve this issue.")
        )
      }, 
      session = app_session) |> 
        suppressWarnings()
    })
    it("warns if there is no user name", {
      withr::local_envvar("GOLEM_CONFIG_ACTIVE" = "default")
      db_path <- withr::local_tempfile(fileext = ".sqlite")
      file.copy(testthat::test_path("fixtures/testdb.sqlite"), db_path)
      app_session <- MockShinySession$new()
      app_session$options$golem_options <- list(
        "meta" = metadata,
        "data" = clinsightful_data,
        "user_db" = db_path
      )
      local_mocked_bindings(
        authenticate_server = function(...) {
          reactiveValues(
            user = "",
            name = "",
            roles = get_roles_from_config()[2],
            sites = data.frame("site_code" = "Site 01", "region" = "DEU")
          )
        }
      )
      
      testServer(app_server, {
        ns <- session$ns
        session$flushReact()
        expect_equal(
          user_error(),
          paste0("No valid user name provided. Functionality is limited. ",
                 "Please contact the administrator to resolve this issue.")
        )
      }, 
      session = app_session) |> 
        suppressWarnings()
    })
  }
)
