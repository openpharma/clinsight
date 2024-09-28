describe(
  "mod_query_follow_up. Feature 1 | Load application module in isolation.", 
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_query_follow_up_ui(id = "test")
      golem::expect_shinytag(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_query_follow_up_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        r = reactiveValues(),
        selected_query = reactiveVal("ID1-unique_id"),
        db_path = ""
      ) 
      testServer(mod_query_follow_up_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    }
    )
  }
)

describe(
  "mod_query_follow_up. Feature 2 | Create follow-up query. 
    As a user, I want to be able to create a follow-up message on a 
    selected query. The follow-up message should be saved in the 
    review database and should be the same as the message in-memory. ", 
  {
    it(
      "Scenario 1 - Add query follow-up message. Given a query database 
          containing a row with [query] set to 'Query text test.' and [query_id] 
          set to 'ID1-unique_id',  
          and [selected_query] set to 'ID1-unique_id',
          and the input [query_follow_up_text] set to 'Test Follow-up message',
          and [user_name] set to 'Admin test' and [user_role] to 'Medical Monitor',
          and the input value [resolved] set to 'FALSE',
          and the input [query_add_follow_up] incremented to 1 to save the query,
          I expect that the follow-up message will be written in the remote and 
          in-memory database", 
      { 
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "Admin test",
            user_role = "Medical Monitor",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal(),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          selected_query("ID1-unique_id")
          session$setInputs(
            query_follow_up_text = "Test Follow-up message",
            resolved = FALSE,
            query_add_follow_up = 1
          )
          saved_data <- with(r$query_data, r$query_data[query_id == "ID1-unique_id", ])
          expect_equal(saved_data$query, c("Query text test.", "Test Follow-up message"))
          expect_equal(saved_data$n, 1:2)
          expect_equal(saved_data$resolved, c("No", "No"))
          
          saved_db_data <- dplyr::tbl(con, "query_data") |> dplyr::collect()
          expect_equal(dplyr::as_tibble(r$query_data), saved_db_data)
        })
      }
    )
  }
)

describe(
  "mod_query_follow_up. Feature 3 | Add follow-up query, mark query as resolved. 
    As a user, I want to be able to add a follow-up 
    query and mark a query as resolved. The information should be saved in the 
    query database.", 
  {    
    it(
      "Scenario 1 - Add query follow-up and resolve query. Given a query database 
          containing a row with [query] set to 'Query text test.' 
          and [query_id] set to 'ID1-unique_id',  
          and [selected_query] set to 'ID1-unique_id',
          and the input [query_follow_up_text] set to 'Test Follow-up message',
          and [user_name] set to 'Admin test' and [user_role] to 'Medical Monitor',
          and the input value [resolved] set to 'TRUE',
          and the input [query_add_follow_up] incremented to 1 to save the query,
          I expect that the value [query] with query_id 'ID1-unique_id' 
          is a vector with the two strings 'Query text test.' and 'Test Follow-up message',
          and that the follow-up number [n] of the same query_id is a vector with '1' and '2',  
          and that the [resolved] value is 'Yes',
          and that a valid date is written in the [resolved_date],
          and that the in-memory [query_data] data frame contains the same query timestamp, 
          and the same query text, and the same query reviewer name and reviewer 
          role as in the query database,  
          and that the database [resolved] values are 'No' for the row with the 
          initial query and 'Yes' for the follow-up query.", 
      {
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "Admin test",
            user_role = "Medical Monitor",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal(),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          selected_query("ID1-unique_id")
          session$setInputs(
            query_follow_up_text = "Test Follow-up message",
            resolved = TRUE,
            query_add_follow_up = 1
          )
          saved_data <- with(r$query_data, r$query_data[query_id == "ID1-unique_id", ])
          expect_equal(saved_data$query, c("Query text test.", "Test Follow-up message"))
          
          expect_equal(saved_data$n, 1:2)
          # all resolved values of the specific queries are overwritten to yes
          expect_equal(saved_data$resolved, c("Yes", "Yes"))
          # we cannot fix the resolved date because it will always be the current date-time
          expect_true(!is.na(as.POSIXct(unique(saved_data$resolved_date))))
          saved_db_data <- dplyr::tbl(con, "query_data") |> dplyr::collect() |> 
            dplyr::filter(query_id == "ID1-unique_id")
          expect_equal(saved_db_data$resolved, c("No", "Yes"))
          expect_equal(saved_db_data$reviewer, saved_data$reviewer)
          expect_equal(saved_db_data$timestamp, saved_data$timestamp)
          expect_equal(saved_db_data$query, saved_data$query)
        })
      }
    )
  }
)

describe(
  "mod_query_follow_up. Feature 4 | Write multiple query follow-up messages. 
    As a user, I want that multiple follow-up messages for a query can be 
    written and saved in the query database, and that the order 
    of the follow-up messages remains as expected.", 
  {
    it(
      "Scenario 1 - Saving multiple query follow-up messages. Given a query database 
      containing a row with [query] set to 'Query text test.' 
      and [query_id] set to 'ID1-unique_id',  
      and [selected_query] set to 'ID1-unique_id', 
      and [user_name] set to 'Admin test' and [user_role] to 'Medical Monitor',
      and the input [query_follow_up_text] set to 'FU message 1',  
      and [resolved] set to 'FALSE', 
      and the input [query_add_follow_up] incremented to 1 to save the query,
      and the input [query_follow_up_text] set to 'FU message 2' ,
      and [resolved] set to 'TRUE',
      I expect that the value [query] with query_id 'ID1-unique_id' 
      is a vector with three strings ('Query text test.'  'FU message 1' and 'FU message 2'),
      and that the follow-up number [n] of the same query_id is a vector with '1', '2', and '3',   
      and that the in-memory [resolved] value is 'Yes',
      and that a valid date is written in the [resolved_date],
      and that both the in-memory [query_data] data frame and the query database 
      contain the same query timestamp, 
      and the same query text, and the same query reviewer name and reviewer role,
      and that the database [resolved] values are 'No' for the first two rose 
      and 'Yes' for the final follow-up query.", 
      {
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "Admin test",
            user_role = "Medical Monitor",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal("ID1-unique_id"),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          session$setInputs(
            query_follow_up_text = "FU message 1",
            resolved = FALSE,
            query_add_follow_up = 1
          )
          session$setInputs(
            query_follow_up_text = "FU message 2",
            resolved = TRUE,
            query_add_follow_up = 2
          )
          session$flushReact()
          saved_data <- with(r$query_data, r$query_data[query_id == "ID1-unique_id", ])
          expect_equal(
            saved_data$query, 
            c("Query text test.", "FU message 1", "FU message 2")
          )
          expect_equal(saved_data$n, 1:3)
          
          # all resolved values of the specific queries are overwritten to yes
          expect_equal(saved_data$resolved, c("Yes", "Yes", "Yes"))
          # we cannot fix the resolved date because it will always be the current date-time
          expect_true(!is.na(as.POSIXct(unique(saved_data$resolved_date))))
          saved_db_data <- dplyr::tbl(con, "query_data") |> dplyr::collect() |> 
            dplyr::filter(query_id == "ID1-unique_id")
          expect_equal(saved_db_data$resolved, c("No", "No", "Yes"))
          expect_equal(saved_db_data$reviewer, saved_data$reviewer)
          expect_equal(saved_db_data$timestamp, saved_data$timestamp)
          expect_equal(saved_db_data$query, saved_data$query)
        })
      }
    )
  }
)

describe(
  "mod_query_follow_up. Feature 5 | Only allow to write follow-up query if query 
    id matches to one in database. 
    As a user, I want that no follow-up query will be written to the database 
    and that the in-memory query information remains the same if it is unclear 
    which query the follow-up message concerns.", 
  {
    it(
      "Scenario 1 - No follow-up query without selected query id. Given a specific [query_follow_up_text], 
          and no query id set in [selected_query],
          I expect that the query database and internal query data frame 
          remain the same.", 
      {
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "Admin test",
            user_role = "Medical Monitor",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal(),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          session$setInputs(
            query_follow_up_text = "Test Follow-up message",
            resolved = TRUE,
            query_add_follow_up = 1
          )
          expect_equal(query_df, r$query_data)
          saved_db_data <- dplyr::collect(dplyr::tbl(con, "query_data"))
          expect_equal(saved_db_data, dplyr::as_tibble(query_df))
          
          selected_query("")
          session$setInputs(query_add_follow_up = 2)
          expect_equal(query_df, r$query_data)
          saved_db_data <- dplyr::collect(dplyr::tbl(con, "query_data"))
          expect_equal(saved_db_data, dplyr::as_tibble(query_df))
        })
      }
    )
    it(
      "Scenario 2 - No follow-up query with an unknown query id. Given a specific [query_follow_up_text], 
          and the query id in [selected_query()] is set to an unknown query_id 
          which is not in the database,
          I expect that no data will be written to the database,
          and that a warning will be given", 
      {
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "Admin test",
            user_role = "Medical Monitor",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal(),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          selected_query("Unkown_ID")
          session$setInputs(
            query_follow_up_text = "Test Follow-up message",
            resolved = TRUE,
            query_add_follow_up = 1
          )
          expect_equal(query_df, r$query_data)
          saved_db_data <- dplyr::collect(dplyr::tbl(con, "query_data"))
          expect_equal(saved_db_data, dplyr::as_tibble(query_df))
          expect_error(output[["query_error"]], "Query ID unknown")
        })
      }
    )
  }
)

describe(
  "mod_query_follow_up. Feature 6 | Only allow to write follow-up query with 
    valid user name and user role. 
    As a user, I want that no follow-up query will be written to the database 
    if no valid user name or user role is available.", 
  {
    it(
      "Scenario 1 - No user name available. 
      follow-up query without selected query id. Given no user name is available, 
          and [query_follow_up_text] contains a specific follow-up text, 
          and [selected_query] is set to 'ID1-unique_id',
          and trying to save a follow-up query by pressing [query_add_follow_up],
          I expect that [query_error] shows an informative error,
          and I expect that the query database and internal query data frame 
          remain the same.", 
      {
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "",
            user_role = "Medical Monitor",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal("ID1-unique_id"),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          session$setInputs(
            query_follow_up_text = "Test Follow-up message",
            resolved = TRUE,
            query_add_follow_up = 1
          )
          
          expect_error(
            output$query_error, 
            "User name missing. Cannot save query anonymously"
          )
          expect_equal(r$query_data, query_df)
          expect_equal(
            DBI::dbGetQuery(con, "SELECT * FROM query_data"), 
            query_df
          )
        })
      }
    )
    it(
      "Scenario 2 - No user role available. Given the same a conditions as in Scenario 1,
          but now with [user_name] set to 'test user' 
          and no [user_role] available, 
          and trying to save a follow-up query by pressing [query_add_follow_up],
          I expect that no data will be written to the database,
          and that a warning will be given", 
      {
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "test user",
            user_role = "",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal("ID1-unique_id"),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          session$setInputs(
            query_follow_up_text = "Test Follow-up message",
            resolved = TRUE,
            query_add_follow_up = 1
          )
          
          expect_error(
            output$query_error, 
            "User role missing. Cannot save query without user role"
          )
          expect_equal(r$query_data, query_df)
          expect_equal(
            DBI::dbGetQuery(con, "SELECT * FROM query_data"), 
            query_df
          )
        })
      }
    )
  }
)


describe(
  "mod_query_follow_up. Feature 7 | Verify follow-up query correctly being 
    written in database. 
    As a user, I want to be able to see an error
    message if the latest query entry does not match the one in the database
    after saving a new entry.",
  {
    it(
      "Scenario 1 - Database save function not working. 
        Given a test data base, 
        and the function 'db_save' being mocked (temporarily replaced) with a
        function that does not write to the database,
        and [user_name] set to 'Admin test',
        and [active_form] to 'Adverse events',
        and [subject_id] to 'ID1',
        and [query_follow_up_text] set to 'Test Follow-up message',
        and [query_add_follow_up] to 1,
        I expect that [save_review_error] is TRUE,
        and that the new query is not save in memory,
        and that [query_data] remains the same as the input test query data.", 
      {
        query_df <- readRDS(test_path("fixtures", "query_testdata.rds"))
        temp_path <- withr::local_tempfile(fileext = ".sqlite") 
        con <- get_db_connection(temp_path)
        DBI::dbWriteTable(con, "query_data", query_df)
        
        local_mocked_bindings(
          db_save = function(...) "nothing written to database"
        )
        testargs <- list(
          r = reactiveValues(
            query_data = query_df,
            user_name = "Admin test",
            user_role = "Medical Monitor",
            subject_id = "ID1"
          ),
          selected_query = reactiveVal(),
          db_path = temp_path
        ) 
        testServer(mod_query_follow_up_server, args = testargs , {
          ns <- session$ns
          selected_query("ID1-unique_id")
          session$setInputs(
            query_follow_up_text = "Test Follow-up message",
            resolved = FALSE,
            query_add_follow_up = 1
          )
          saved_data <- with(r$query_data, r$query_data[query_id == "ID1-unique_id" & n == 2, ])
          expect_true(query_save_error())
          expect_equal(nrow(saved_data), 0)
          expect_equal(r$query_data, dplyr::as_tibble(query_df))
          saved_db_data <- dplyr::tbl(con, "query_data") |> dplyr::collect()
          expect_equal(saved_db_data, dplyr::as_tibble(query_df))
        })
      })
  }
)
