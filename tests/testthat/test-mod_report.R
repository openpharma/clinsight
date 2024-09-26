describe("mod_report. Feature 1 | As a user, I want to be able to start the 
  module in isolation", {
    it("Can load the module UI, with functioning base paramneters and internals.", {
      ui <- mod_report_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_study_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        r = reactiveValues(),
        rev_data = reactiveValues(),
        db_path = ""
      ) 
      testServer(mod_report_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  })

describe(
  "mod_report. Feature 2 | As a user, I want to be able to create a PDF report that 
  contains my review activities of that session.", 
  {
    it(
      "Scenario 1 - Report data preparation. 
          Given data frames [query_data] and [review_data], 
          and all values in column 'Reviewed' in [review_data] are 'Yes',
          and the get_review_data is set to 1, 
          I expect that the internal data frames are correctly prepared for 
          reporting.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        query_df <- data.frame(
          "query_id"      = c("ID1-unique_id", "ID2-unique_id"),
          "subject_id"    = c("ID1", "ID2"),
          "type"          = c("Major", "Normal"),
          "event_label"   = c("Visit 1"),
          "item_group"    = c("Vital signs", "Adverse events"),
          "item"          = c("Pulse", "Sepsis"),
          "timestamp"     = c("2023-11-02 01:01:01 UTC", "2023-11-03 01:01:01 UTC"),
          "n"             = c(1),     
          "reviewer"      = c("Admin (Medical Monitor)", "Author 2 (Administrator)"),
          "query"         = c("Query text test.", "Scoring correct? Please verify"),
          "resolved"      = c("No"),
          "resolved_date" = c(""),
          "edit_reason"   = c("")
        ) |> 
          dplyr::as_tibble()
        DBI::dbWriteTable(con, "query_data", query_df)
        review_df <- data.frame(
          subject_id = "Test_name",
          event_name = "Visit 1",
          item_group = "Test_group",
          form_repeat = 1,
          item_name = "Test_item",
          event_date = "2023-11-01",
          edit_date_time = "2023-11-05 01:26:00",
          reviewed = "Yes",
          comment = "",
          reviewer = "Admin (Medical Monitor)",
          timestamp = "2023-11-13 01:01:01",
          status = "old"
        )
        DBI::dbWriteTable(con, "all_review_data", review_df)
        
        testargs <- list(
          r = reactiveValues(
            review_data = review_df,
            user_name = "Admin",
            user_role = "Medical Monitor"
          ),
          rev_data = reactiveValues(summary = reactive({review_df[0,]})), 
          db_path = temp_path
        ) 
        
        testServer(mod_report_server, args = testargs, {
          ns <- session$ns
          expect_equal(forms_missing_review(), 0)
          session$setInputs(
            create_report = 1, 
            report_type = "session", 
            include_from_date = "2023-10-01"
          )
          expect_equal(
            selected_review_data(), 
            review_df |> 
              dplyr::select(
                "ID" = subject_id, 
                "Form" = item_group, 
                "Event" = event_name, 
                "Edit date" = edit_date_time, 
                "Reviewer" = reviewer, 
                "Time" = timestamp, 
                comment
              ) |> 
              dplyr::distinct() |> 
              dplyr::as_tibble()
          )
          expect_equal(
            selected_query_data(),
            query_df[1,] |> 
              dplyr::select(
                query_id, 
                "ID" = subject_id, 
                "Type" = type,
                "Form" = item_group, 
                "Item" = item, 
                "Event" = event_label, 
                "Time" = timestamp, 
                "Query" = query, 
                "Author" = reviewer,
                resolved
              )
          )
          
        })
      }
    )
    it(
      "Scenario 2 - Warn for missing reviews. 
          Given data frames [query_data] and [review_data], 
          and some values in column 'Reviewed' in [review_data] are 'No',
          and the get_review_data is set to 1, 
          and check_missing_data is set to 1,
          I expect that opening the missing data modal is trigggered,
          by incrementing the rev_data$open_modal counter with one.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        query_df <- data.frame(
          "query_id"      = c("ID1-unique_id", "ID2-unique_id"),
          "subject_id"     = c("ID1", "ID2"),
          "type"          = c("Major", "Normal"),
          "event_label"   = c("Visit 1"),
          "item_group"    = c("Vital signs", "Adverse events"),
          "item"          = c("Pulse", "Sepsis"),
          "timestamp"     = c("2023-01-01 01:01:01 UTC", "2023-11-02 01:01:01 UTC"),
          "n"             = c(1),     
          "reviewer"  = c("Test author", "Author3"),
          "query"         = c("Query text test.", "Scoring correct? Please verify"),
          "resolved"      = c("No", "Yes"),
          "resolved_date" = c("", "2023-11-15 01:01:01"),
          "edit_reason"   = c("", "")
        ) |> 
          dplyr::as_tibble()
        DBI::dbWriteTable(con, "query_data", query_df)
        review_df <- data.frame(
          subject_id = "Test_name",
          event_name = "Visit 1",
          item_group = "Test_group",
          form_repeat = 1,
          item_name = "Test_item",
          event_date = "2023-11-01",
          edit_date_time = "2023-11-05 01:26:00",
          reviewed = "No",
          comment = "",
          reviewer = "",
          timestamp = "",
          status = "new"
        )
        DBI::dbWriteTable(con, "all_review_data", review_df)
        
        testargs <- list(
          r = reactiveValues(review_data = review_df),
          rev_data = reactiveValues(summary = reactive({review_df})), 
          db_path = temp_path
        ) 
        
        summarize_review_data(review_df)
        
        testServer(mod_report_server, args = testargs, {
          ns <- session$ns
          expect_equal(forms_missing_review(), 1)
          session$setInputs(create_report = 1, check_missing_data = 1)
          expect_equal(rev_data$open_modal, 1)
          expect_equal(query_data(), NULL)
          expect_equal(review_data(), NULL)
          # incomplete data will be shown after incrementing get_incomplete_report
          session$setInputs(get_incomplete_report = 1)
          expect_true(is.data.frame(query_data()))
          expect_true(is.data.frame(review_data()))
        })
      }
    )
    
    it(
      "Scenario 3 - Download report. 
          Given data frames [query_data] and [review_data], 
          and all values in column 'Reviewed' in [review_data] are 'Yes',
          and the get_review_data is set to 1, 
          and the user is set to 'Test user (Medical Monitor)',
          I expect that I can successfully download a PDF report 
          containing the review activities.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        query_df <- data.frame(
          "query_id"      = c("ID1-unique_id", "ID2-unique_id"),
          "subject_id"    = c("ID1", "ID2"),
          "type"          = c("Major", "Normal"),
          "event_label"   = c("Visit 1"),
          "item_group"    = c("Vital signs", "Adverse events"),
          "item"          = c("Pulse", "Sepsis"),
          "timestamp"     = c("2023-01-01 01:01:01 UTC", "2023-11-02 01:01:01 UTC"),
          "n"             = c(1),     
          "reviewer"  = c("Test user (Medical Monitor)", "Test user (Medical Monitor)"),
          "query"         = c("Query text test.", "Scoring correct? Please verify"),
          "resolved"      = c("No", "Yes"),
          "resolved_date" = c("", "2023-11-15 01:01:01"),
          "edit_reason"   = c("", "")
        ) |> 
          dplyr::as_tibble()
        DBI::dbWriteTable(con, "query_data", query_df)
        review_df <- data.frame(
          subject_id = "Test_name",
          event_name = "Visit 1",
          item_group = "Test_group",
          form_repeat = 1,
          item_name = "Test_item",
          event_date = "2023-11-01",
          edit_date_time = "2023-11-05 01:26:00",
          reviewed = "Yes",
          comment = "",
          reviewer = "Test user (Medical Monitor)",
          timestamp = "2023-11-13 01:01:01",
          status = "old"
        )
        DBI::dbWriteTable(con, "all_review_data", review_df)
        
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page(
              title = "Test report creation", 
              mod_report_ui(id = "test-report")
            )
          )
        }
        
        test_server <- function(input, output, session){
          mod_report_server(
            id = "test-report", 
            r = reactiveValues(
              review_data = review_df, 
              user_name = "Test user", 
              user_role = "Medical Monitor",
              filtered_data = get_appdata(clinsightful_data)
            ),
            rev_data = reactiveValues(summary = reactive({review_df[0,]})), 
            db_path = temp_path, 
            table_names = setNames(
              metadata$table_names$raw_name,
              metadata$table_names$raw_name
            )
          )
        }
        
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "mod_report",
          width = 1619, 
          height = 955, 
          timeout = 10000
        )
        withr::defer(app$stop())
        
        app$click("test-report-create_report")
        app$wait_for_idle(800)
        app$set_inputs("test-report-include_from_date" = "2023-01-01")
        app$wait_for_idle()
        app$expect_values()
        
        pdf_report_path <- app$get_download("test-report-report")
        # expect that a pdf file is downloaded:
        expect_equal(basename(pdf_report_path), "report.pdf")
        # expect a pdf size greater than 10kB:
        expect_gt(file.size(pdf_report_path), 10000)
      }
    )
  }
)
