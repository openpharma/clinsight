describe(
  "mod_queries. Feature 1 | Load application module in isolation.", 
  {
    query_df <- data.frame(
      "subject_id"     = c("ID1"),
      "event_label"   = c("Visit 1"),
      "item_group"    = c("Vital signs", "Vital signs", "Adverse events"),
      "item"          = c("Pulse", "Pulse", "Sepsis"),
      "timestamp"     = c("2023-01-01 01:01:01 UTC", "2023-11-01 01:01:01 UTC", 
                          "2023-11-02 01:01:01 UTC"),
      "query_id"      = c("ID1-unique_id", "ID1-unique_id", "ID2-unique_id"),
      "n"             = c(1, 2, 1),     
      "reviewer"      = c("Test author", "Author2", "Author3"),
      "query"         = c("Query text test.", "Query follow-up text", 
                          "Scoring correct? Please verify"),
      "resolved"      = c("Yes", "Yes", "No"),
      "resolved_date" = c("2023-11-01 01:01:01 UTC", "2023-11-01 01:01:01 UTC", ""),
      "edit_reason"   = c("")
    )
    
    testargs <- list(
      r = reactiveValues(query_data = query_df, user_name = "Admin test"),
      navinfo = reactiveValues(),
      all_forms = data.frame(),
      db_path = ""
    ) 
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_queries_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_study_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_queries_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_queries. Feature 2 | View queries. As a user, I want to be able to view 
  all queries in a table if show_resolved is set to TRUE, ordered by query 
  resolved status (open queries first), and query type (major queries on top) . 
  If show_resolved is set to FALSE, only open queries will be shown.", {
    it(
      "Scenario 1 - Show queries. Given a data frame [query_df],
        and [show_resolved] is set to 'TRUE',
        I expect the initial queries to be shown in 'initial_queries' ordered 
        by resolved status and query type, 
        and if [show_resolved] is set to 'TRUE',
        I expect that only the open queries are shown in 'initial_queries', 
        ordered by query type (major queries on top).", 
      {
        query_df <- data.frame(
          "subject_id"     = c("ID1"),
          "type"          = c("Normal", "Normal", "Normal", "Major"),
          "event_label"   = c("Visit 1"),
          "item_group"    = c("Vital signs", "Vital signs", "Adverse events", "Medication"),
          "item"          = c("Pulse", "Pulse", "Sepsis", "Oxycodone"),
          "timestamp"     = c("2023-01-01 01:01:01 UTC", "2023-11-01 01:01:01 UTC", 
                              "2023-11-02 01:01:01 UTC", "2023-11-03 01:01:01 UTC"),
          "query_id"      = c("ID1-unique_id", "ID1-unique_id", "ID2-unique_id", "ID3-unique_id"),
          "n"             = c(1, 2, 1, 1),     
          "reviewer"      = c("Test author", "Author2", "Author3", "Author4"),
          "query"         = c("Query text test.", "Query follow-up text", 
                              "Scoring correct? Please verify", "Major query!"),
          "resolved"      = c("Yes", "Yes", "No", "No"),
          "resolved_date" = c("2023-11-01 01:01:01 UTC", "2023-11-01 01:01:01 UTC", "", ""),
          "edit_reason"   = c("")
        )
        testargs <- list(
          r = reactiveValues(query_data = query_df, user_name = "Admin test"),
          navinfo = reactiveValues(),
          all_forms = data.frame(),
          db_path = ""
        ) 
        testServer(mod_queries_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_resolved = TRUE)
          expect_equal(initial_queries(), dplyr::filter(query_df, n == 1) |> 
                         dplyr::arrange(resolved, type ) )
          session$setInputs(show_resolved = FALSE)
          session$flushReact()
          expected_results <- query_df[query_df$resolved == "No", ] |> 
            dplyr::arrange(type) 
          rownames(expected_results) <- as.integer(1:2)
          expect_equal(initial_queries(),  expected_results)
        })
      }
    )
  }
)


describe(
  "mod_queries. Feature 3 | As a user, I want to be able to view all the 
    follow-up messages that have been written for a selected query. 
    I want to be able to click follow-up, to write a follow-up message to 
    the query.", 
  {
    query_df <- data.frame(
      "subject_id"    = c("ID1"),
      "type"          = c("Normal"),
      "event_label"   = c("Visit 1"),
      "item_group"    = c("Vital signs", "Vital signs", "Adverse events"),
      "item"          = c("Pulse", "Pulse", "Sepsis"),
      "timestamp"     = c("2023-01-01 01:01:01 UTC", "2023-11-01 01:01:01 UTC", 
                          "2023-11-02 01:01:01 UTC"),
      "query_id"      = c("ID1-unique_id", "ID1-unique_id", "ID2-unique_id"),
      "n"             = c(1, 2, 1),     
      "reviewer"      = c("Test author", "Author2", "Author3"),
      "query"         = c("Query text test.", "Query follow-up text", 
                          "Scoring correct? Please verify"),
      "resolved"      = c("Yes", "Yes", "No"),
      "resolved_date" = c("2023-11-01 01:01:01 UTC", "2023-11-01 01:01:01 UTC", ""),
      "edit_reason"   = c("")
    )
  
    it(
      "Scenario 2 - Select query with follow-up messages. 
        Given a data frame with test query data, 
        and with the selected row [queries_row_selected] set to '2', 
        and show_resolved set to 'TRUE',
        I expect [selected_query] to be the selected [query_id] 'ID1-unique_id', 
        and that the [selected_query_data] data frame shows the query and its 
        follow-up query, as stored in the test query data, 
        and the selected query title contains the item name, subject_id, 
        item_group, and resolved value. ", {
          testargs <- list(
            r = reactiveValues(query_data = query_df, user_name = "Admin test"),
            navinfo = reactiveValues(),
            all_forms = data.frame(),
            db_path = ""
          ) 
          testServer(mod_queries_server, args = testargs, {
            ns <- session$ns
            session$setInputs(
              queries_rows_selected = 2,
              show_resolved = TRUE
              )
            expect_equal(selected_query(), "ID1-unique_id")
            expected_result <- query_df[1:2,] |> 
              dplyr::mutate(reviewer = paste0(reviewer, " ", timestamp))
            expect_equal(selected_query_data(), expected_result)
            expect_equal(
              output[["selected_query_title"]],
              "<b>ID1: Pulse</b><br>Vital signs, Visit 1 (resolved)"
            )
          })
        }
    )
    it(
      "Scenario 3 - Selected query that is unresolved. Given a data frame [query_df], 
       and with the selected row [queries_row_selected] set to '1', 
       and show_resolved set to 'TRUE',
          I expect [selected_query] to be the selected query_id 'ID2-unique_id', 
          and that [selected_query_data] shows the correct query belonging to the 
          selected query_id, 
          and that the selected query title contains the item name, subject_id, 
          item_group, and will not show that it is resolved.", {
            testargs <- list(
              r = reactiveValues(query_data = query_df, user_name = "Admin test"),
              navinfo = reactiveValues(),
              all_forms = data.frame(),
              db_path = ""
            ) 
            testServer(mod_queries_server, args = testargs, {
              ns <- session$ns
              session$setInputs(queries_rows_selected = 1, show_resolved = TRUE)
              expect_equal(selected_query(), "ID2-unique_id")
              expected_result <- query_df[3,] |> 
                dplyr::mutate(reviewer = paste0(reviewer, " ", timestamp))
              expect_equal(selected_query_data(), expected_result)
              expect_equal(
                output[["selected_query_title"]],
                "<b>ID1: Sepsis</b><br>Adverse events, Visit 1"
              )
            })
          }
    )
  }
)

