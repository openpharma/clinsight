describe(
  "collect_query_data() works", 
  {
    it("collects data from a database without errors", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      queries <-  data.frame(
        subject_id   = "885",
        event      = "Any visit", 
        group      = "Adverse events", 
        item_name   = "AE Number",
        timestamp = "2023-11-01 01:01:01",
        query_id   = "unique_id",
        n          =  1,
        reviewer = "Test-author",
        query = "Initial query text", 
        resolved = "No",
        resolved_date = NA_character_,
        edit_reason = ""
      ) |> 
        dplyr::as_tibble() # for easier comparisons, since db_slice_rows returns a tibble
      db_temp_connect(temp_path, {
        DBI::dbWriteTable(con, "query_data", queries, overwrite = TRUE)
      })
      expect_equal(
        collect_query_data(temp_path, "query_data"), 
        queries
      )
    })
    
    it(
      paste0(
        "Selects the latest timestamp by the required groups (query_id and n ", 
        "(n= number showing follow-up message) ). This ensures that a query", 
        " can be edited, and that only the latest (edited) result will be returned."), 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        queries_two_timestamps <-  data.frame(
          subject_id   = "885",
          event      = "Any visit", 
          group      = "Adverse events", 
          item_name   = "AE Number",
          timestamp = c("2023-11-01 01:01:01", "2023-11-10 01:01:01"), 
          query_id   = "unique_id",
          n          =  1,
          reviewer = "Test-author",
          query = c("Initial query text", "corrected query text"), 
          resolved = "No", 
          resolved_date = NA_character_,
          edit_reason = ""
        ) |> 
          dplyr::as_tibble() 
        db_temp_connect(temp_path, {
          DBI::dbWriteTable(con, "queries_two_timestamps", queries_two_timestamps)
        })
        
        expect_true(is.data.frame(collect_query_data(temp_path, "queries_two_timestamps")))
        expect_equal(
          collect_query_data(temp_path, "queries_two_timestamps"), 
          queries_two_timestamps[2, ]
        )
      }
    )
    
    it("set all colums to resolved with the equivalent resolved date ", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      queries_set_resolved <-  data.frame(
        subject_id   = "885",
        event      = "Any visit", 
        group      = "Adverse events", 
        item_name   = "AE Number",
        timestamp = c("2023-11-01 01:01:01", "2023-11-10 01:01:01"), 
        query_id   = "unique_id",
        n          =  c(1, 2),
        reviewer = c("Test-author", "Different author"),
        query = c("Initial query text", "correccted query text"), 
        resolved = c("No", "Yes"), 
        resolved_date = c("", "2023-11-10 01:01:02"),
        edit_reason = ""
      ) |> 
        dplyr::as_tibble() 
      db_temp_connect(temp_path, {
        DBI::dbWriteTable(con, "queries_set_resolved", queries_set_resolved)
      })
      expect_equal(
        collect_query_data(temp_path, "queries_set_resolved"), 
        queries_set_resolved |> 
          dplyr::mutate(
            resolved = "Yes",
            resolved_date = "2023-11-10 01:01:02"
          )
      )
    })
    
  }
)

