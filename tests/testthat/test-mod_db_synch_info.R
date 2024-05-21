describe(
  "mod_db_synch_info. Feature 1 | As a user, I want to be able to run the 
  module in isolation", 
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_db_synch_info_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_study_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    # Set up server module parameters here
    testargs <- list(
      app_data = list(),
      db_path = "",
      test_mode = TRUE
    ) 
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_db_synch_info_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_db_synch_info. Feature 2 | DB synch date information. As a user, I want to
  be able to see the latest date at which the database is updated and at which
  new data was entered in the EDC.",
  {
    # See also database update date.
    it(
      "Scenario 1 | DB synch date and EDC update date. 
      Given current date set to 2024-01-10,
       and db_synch_time to 2024-01-10,
       and edc_latest_date to 2024-01-10,
       I expect that ['synch_warning'] is empty,
       and that [edc_latest_date] is '2024-01-10',
       and that [synch_time] is '2024-01-10',
       and that the output ['db_synch_info'] contains  
       'EDC Sync date 2024-01-10' and 'EDC latest data 2024-01-10'", {
         temp_path <- withr::local_tempfile(fileext = ".sqlite")
         db_temp_connect(temp_path, {
           DBI::dbWriteTable(con, "db_synch_time", data.frame("synch_time" = "2024-01-10"))
         })

         testargs <- list(
           app_data = list("Form1" = data.frame("site_code" = "", "edit_date_time" = "2024-01-10")), 
           db_path = temp_path,
           test_mode = TRUE
         )
         
         testServer(mod_db_synch_info_server, args = testargs, {
           ns <- session$ns
           expect_equal(synch_warning(), "")
           expect_equal(edc_latest_date(), "2024-01-10")
           expect_equal(synch_time(), "2024-01-10")
           expect_equal(
             output[["db_synch_info"]], 
             "EDC Sync date: <br>2024-01-10<br><br>EDC latest data: 2024-01-10"
           )
         })
       }
    )
  }
)

describe("mod_db_synch_info. Feature 3 | DB out of synch. As a user, I want to get a
  warning if the database synchronization did not happen on the same day as the data review.", {
    it(
      "Scenario 1 | Old synchronization. Given the current date set to 2024-01-10,
       and the latest DB synch date set to '2024-01-08',
       and the latest EDC udpate date to '2024-01-01',
       I expect that a pop-up window will be shown with a warning 
       'Latest update was 2 days ago (2024-01-08)',
       synchronization was more than a day old", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        db_temp_connect(temp_path, {
          DBI::dbWriteTable(con, "db_synch_time", 
                            data.frame("synch_time" = "2024-01-08"))
        })
        test_ui <- function(request){
          bslib::page_navbar(sidebar = mod_db_synch_info_ui("test"))
        }
        test_server <- function(input, output, session){
          mod_db_synch_info_server(
            id = "test", 
            app_data = list("Form1" = data.frame("site_code" = "", "edit_date_time" = "2024-01-01")), 
            db_path = temp_path,
            test_mode = TRUE
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "test-mod_db_synch_info_s1",
          width = 1619, 
          height = 955
        )
        withr::defer(app$stop())
        app$expect_values()
      }
    )
    it(
      "Scenario 2 | Synchronization date or EDC update date missing. Given the 
        current date set to 2024-01-10,
        and the latest DB synch date is either missing or 'NULL',
        and the latest EDC update date is set to '2024-01-01',
        I expect that a pop-up window will be shown with the warning 
        'The latest database synchronization date is unknown',
        and the warning 'the most recent EDC data entry is Unknown'", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        db_temp_connect(temp_path, {
          DBI::dbWriteTable(con, "db_synch_time", data.frame("synch_time" = ""))
        })
        test_ui <- function(request){
          bslib::page_navbar(sidebar = mod_db_synch_info_ui("test"))
        }
        test_server <- function(input, output, session){
          mod_db_synch_info_server(
            id = "test", 
            app_data = list("Form1" = data.frame("site_code" = "", "edit_date_time" = "")), 
            db_path = temp_path,
            test_mode = TRUE
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "test-mod_db_synch_info_s2",
          width = 1619, 
          height = 955
        )
        withr::defer(app$stop())
        app$expect_values()
      }
    )
  }
)
