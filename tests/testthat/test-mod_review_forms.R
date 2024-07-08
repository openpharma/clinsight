describe(
  "mod_review_forms. Feature 1 | As a user, I want to be able to 
         load the application in isolation. ", 
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_review_forms_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_review_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        r = reactiveValues(
          user_name = reactiveVal("test_name"),
          subject_id = "885",
          review_data = data.frame()
        ),
        active_form = reactiveVal("Adverse events"),
        active_tab = reactiveVal("Common forms"),
        db_path = ""
      )
      testServer(
        mod_review_forms_server, args = testargs, {
          ns <- session$ns
          expect_true(inherits(ns, "function"))
          expect_true(grepl(id, ns("")))
          expect_true(grepl("test", ns("test")))
        })
    })
  }
)

describe(
  paste0("mod_review_forms. Feature 2 | As a user, I want to be able to save ",
         "a review of a form in the database. After saving the review, all items of ", 
         "that form that are not yet reviewed should get a tag that the value was ",
         "reviewed."), 
  {
    it(
      paste0("Scenario 1 | Save a review. Given test review data with at ",
             "least an 'Adverse event' form with patient '885',",
             "and [active_patient] set to '885', ",
             "and [active_form] set to 'Adverse events', ",
             "and [active_tab] set to 'Common forms', ",
             "and [form_reviewed] set to FALSE, ",
             "I expect that I can save a new review properly, ",
             "with the result saved in the application being the same as ", 
             "the one saved in the database."),
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        
        testargs <- list(
          r = reactiveValues(
            user_name = reactiveVal("test_name"),
            subject_id = "885",
            review_data = db_slice_rows(temp_path)
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          db_path = temp_path
        )
        testServer(
          mod_review_forms_server, args = testargs, {
            ns <- session$ns
            session$setInputs(form_reviewed = TRUE, save_review = 1)
            db_reviewdata <- db_temp_connect(db_path, {
              dplyr::tbl(con, "all_review_data") |> 
                dplyr::collect()
            })
            expect_equal(r$review_data, db_slice_rows(db_path))
            # it should have two rows in the DB, one with review= 'No' and the other with reviewed = "Yes"
            expect_equal(with(db_reviewdata, reviewed[subject_id == "885"]), c("No", "Yes") )
            expect_snapshot({
              print(dplyr::select(r$review_data, -timestamp), width = Inf)
            })
            Sys.sleep(1) # because the timestamp only records seconds, 
            # we should add delay here to prevent that the exact same timestamp is 
            # created in the next step. The timestamp is needed for uniquely selecting the latest entry.
            # It would still work since it defaults to select the last row of the database, 
            # but it might give a warning. 
            
            # It saves a review a second time properly if the review state is changed, 
            # only showing the latest review data in the app but storing all review
            # data, including the previous one, in the database
            # ! if add_comment != logical (1 for example), the test below will fail.
            # two rows with the same editdateetime will be written to the database.
            session$setInputs(
              form_reviewed = FALSE, 
              add_comment = TRUE, 
              review_comment = "test review",
              save_review = 2
              )
            db_reviewdata <- db_temp_connect(db_path, {
              dplyr::tbl(con, "all_review_data") |> 
                dplyr::collect()
            })
            expect_equal(with(db_reviewdata, comment[subject_id == "885"]), c("", "", "test review"))
            expect_equal(with(db_reviewdata, reviewed[subject_id == "885"]), c("No", "Yes", "No"))
            expect_equal(r$review_data, db_slice_rows(db_path))
            expect_snapshot(print(dplyr::select(r$review_data, -timestamp), width = Inf))
          })
      }
    )
    it(
      "Scenario 2 | Save a review. Given a data frame
      and a database with review data with [reviewed] status set to 'new' (not reviewed yet),
        and [user_name] set to 'test_name',
        and [subject_id]  set to '885',
        and [active_form] set to 'Adverse events',
        and first (1) No input is given, then (2) the value [form_review] is set to 'TRUE',
        and then (3) the [save_review] button is clicked,
        I expect that, after each action, the save review button and the option 
        to add a comment will be (1) disabled, (2) enabled, and (3) disabled,
        and that, after the aforementioned input actions, the review status of the active form and
        active subject in the database is set to 'old' (reviewed),
        and the reviewer is set to 'test_name'.",
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page_navbar(sidebar = bslib::sidebar(mod_review_forms_ui("test")))
          )
        }
        test_server <- function(input, output, session){
          mod_review_forms_server(
            id = "test",
            r = reactiveValues(
              user_name = reactiveVal("test_name"),
              subject_id = "885",
              review_data = db_slice_rows(temp_path)
            ),
            active_form = reactiveVal("Adverse events"),
            active_tab = reactiveVal("Common events"),
            db_path = temp_path
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "test-mod_review_forms",
          timeout = 8000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        app$wait_for_idle(2500)
        
        # save button and comment option should not be available:
        app$expect_values()
        expect_true(app$get_js("document.getElementById('test-save_review').disabled;"))
        expect_true(app$get_js("document.getElementById('test-add_comment').disabled;"))
        
        app$click("test-form_reviewed")
        # now the save button and comment option is available:
        app$expect_values()
        expect_false(app$get_js("document.getElementById('test-save_review').disabled;"))
        expect_false(app$get_js("document.getElementById('test-add_comment').disabled;"))
        
        app$click("test-save_review")
        app$wait_for_idle()
        # save button and comment option should not be available anymore:
        app$expect_values()
        expect_true(app$get_js("document.getElementById('test-save_review').disabled;"))
        expect_true(app$get_js("document.getElementById('test-add_comment').disabled;"))
        
        # review status and reviewer is saved as expected
        saved_review_row <- db_slice_rows(temp_path) |>
          dplyr::filter(subject_id == "885")
        expect_equal(saved_review_row$status, "old")
        expect_equal(saved_review_row$reviewer, "test_name")
      }
    )
  }
)

describe(
  "mod_review_forms. Feature 3 | As a user, I want to get feedback on whether a 
  review is needed or not for an active form. The review controls should only be 
  enabled when there is data to review.", 
  {
    it(
      "Scenario 1 | Review needed. Given test review data with at least an 
        'Adverse event' form with patient '885',
        and [active_patient] set to '885',
        and [active_form] set to 'Adverse events',
        and [active_tab] set to 'Common forms',
        and [form_reviewed] set to FALSE,
        I expect that the data frame [active_review_data] contains one row with 
        data of participant '885',
        and with the [item_group] set to 'Adverse events',
        and that a message will be displayed containing the text 'Requires review'", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        testargs <- list(
          r = reactiveValues(
            user_name = reactiveVal("test_name"),
            subject_id = "885",
            review_data = db_slice_rows(temp_path)
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          db_path = temp_path
        )
        
        testServer(mod_review_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(form_reviewed = FALSE)
          expect_equal(
            review_data_active(),
            dplyr::filter(r$review_data, subject_id == "885", item_group == "Adverse events") |>
              dplyr::select(subject_id, item_group, edit_date_time, reviewed, comment, status)
          )
          expect_equal(review_data_active()$item_group, "Adverse events")
          expect_equal(nrow(review_data_active()), 1)
          expect_error(output[["save_review_error"]], "Requires review")
        })
      }
    )
    it(
      "Scenario 2 | Saving with unchanged review status. Given the same 
      conditions as in Scenario 1, and setting comment to 'test comment',
      and attempting to save a review 'save review',
      I expect that [enable_save_review()] is set to 'FALSE',
      and that the review database remains unchanged.",
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        old_review_table <- db_temp_connect(temp_path, {
          DBI::dbGetQuery(con, "SELECT * FROM all_review_data")
        })
        testargs <- list(
          r = reactiveValues(
            user_name = reactiveVal("test_name"),
            subject_id = "885",
            review_data = db_slice_rows(temp_path)
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          db_path = temp_path
        )
        
        testServer(mod_review_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(form_reviewed = FALSE, add_comment = "test comment")
          expect_false(enable_save_review())
          session$setInputs(save_review = 1)
          new_review_table <- db_temp_connect(db_path, {
            DBI::dbGetQuery(con, "SELECT * FROM all_review_data")
          })
          expect_error(output[["save_review_error"]], "Requires review")
          expect_equal(old_review_table, new_review_table)
        })
      }
    )
    it(
      "Scenario 3 | No data to review. Given [active_form] set to a 
        non-existing form named [non-existent],
        and that I try to save a review by setting [save_review] to 2,
        I expect that a warning message will be displayed with the text [Nothing to review],
        and that no new information is saved to the database,
        and that no new information is saved to the app data frame.",
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        testargs <- list(
          r = reactiveValues(
            user_name = reactiveVal("test_name"),
            subject_id = "885",
            review_data = db_slice_rows(temp_path)
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          db_path = temp_path
        )
        testServer(
          mod_review_forms_server, args = testargs, {
            ns <- session$ns
            active_form("Non-existing")
            data_before_save <- r$review_data
            db_before_save <- db_temp_connect(db_path, {
              dplyr::tbl(con, "all_review_data") |> 
                dplyr::collect()
            })
            session$setInputs(save_review = 1)
            expect_error(output[["save_review_error"]], "Nothing to review")
            expect_equal(r$review_data, data_before_save)
            db_reviewdata <- db_temp_connect(db_path, {
              dplyr::tbl(con, "all_review_data") |> 
                dplyr::collect()
            })
            expect_equal(db_reviewdata, db_before_save)
          })
      }
    )
  }
)
describe(
  "mod_review_forms. Feature 4 | As a user, I want that saving data is only 
  possible with a valid user name", 
  {
    it(
      "Scenario 1 | Trying to save data without user name. Given 
        a data frame and a database with review data with [reviewed] status set 
        to 'new' (not reviewed yet), 
        and [subject_id]  set to '885', 
        and [active_form] set to 'Adverse events', 
        and no [user_name] available, 
        and setting the value [form_review] is to 'TRUE' and clicking on the 
        [save_review] button, 
        I expect that an error message will be displayed, 
        and that the review data of the selected subject and form is unaltered.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page_navbar(sidebar = bslib::sidebar(mod_review_forms_ui("test")))
          )
        }
        test_server <- function(input, output, session){
          mod_review_forms_server(
            id = "test", 
            r = reactiveValues(
              user_name = reactiveVal(NULL), 
              subject_id = "885", 
              review_data = db_slice_rows(temp_path)
            ),
            active_form = reactiveVal("Adverse events"),
            active_tab = reactiveVal("Common events"),
            db_path = temp_path
          )
        }
        test_app <- shinyApp(test_ui, test_server, options = list("test.mode" = TRUE))
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "test-mod_review_forms_no_user",
          timeout = 8000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        
        app$wait_for_idle(2500)
        app$click("test-form_reviewed")
        app$click("test-save_review")  
        
        app$wait_for_idle()
        app$expect_values()
        
        # review status and reviewer is saved as expected
        saved_review_row <- db_get_review(
          temp_path, subject = "885", form = "Adverse events"
        )
        expect_equal(saved_review_row$status, "new")
        expect_equal(saved_review_row$reviewer, "")
      }
    )   
    
  }
)

describe(
  "Feature 5 | As a user, I want that data in memory remains the same as the one 
  in the database, even if an error occurs when saving data to the database", 
  {
    it(
      "Scenario 1 | Database save function not working. 
      Given test review data,
      and the function 'db_save_review' being mocked (temporarily replaced) with a
      function that does not write to the database,
      and [user_name] set to '885',
      and [active_form] to 'Adverse events',
      and [active_tab] to 'Common forms',
      and [form_reviewed] to 'TRUE',
      and [save_review] to '1',
      I expect that [save_review_error] is TRUE,
      and that the new review data is not saved in memory,
      and that the rows of [review_data] in the database of subject 885 are 
      still marked as not reviewed", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        file.copy(test_path("fixtures", "review_testdb.sqlite"), temp_path) 
        rev_data <- db_slice_rows(temp_path)
        local_mocked_bindings(
          db_save_review = function(...) "no data saved in database"
        )
        testargs <- list(
          r = reactiveValues(
            user_name = reactiveVal("test_name"),
            subject_id = "885",
            review_data = rev_data
          ),
          active_form = reactiveVal("Adverse events"),
          active_tab = reactiveVal("Common forms"),
          db_path = temp_path
        )
        testServer(
          mod_review_forms_server, args = testargs, {
            ns <- session$ns
            session$setInputs(form_reviewed = TRUE, save_review = 1)
            db_reviewdata <- db_temp_connect(db_path, {
              dplyr::tbl(con, "all_review_data") |> 
                dplyr::collect()
            })
            expect_true(review_save_error())
            expect_equal(r$review_data, rev_data)
            # it should still have one row in the DB with review= 'No'
            expect_equal(with(db_reviewdata, reviewed[subject_id == "885"]), "No" )
          })
      }
    )
  }
)


