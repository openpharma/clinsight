describe(
  "db_create(). Feature 1 | As a user, I want to be able to create an app database that 
  contains the necessary tables for the application to function. ", 
  {
    it("Can create an app database", {
      rev_data <- data.frame(
        subject_id = "Test_name",
        event_name = "Visit 1",
        item_group = "Test_group",
        form_repeat = 1,
        item_name = "Test_item",
        event_date = "2023-11-01",
        edit_date_time = "2023-11-05 01:26:00"
      )
      review_expected <- data.frame(
        reviewed = "No",
        comment = "",
        reviewer = "",
        status = "new"
      )
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      
      db_create(rev_data, temp_path)
      
      con <- get_db_connection(temp_path)
      expect_equal(
        DBI::dbListTables(con), 
        c("all_review_data", "db_synch_time", "query_data")
        )
      expect_equal(
        colnames(dplyr::tbl(con, "all_review_data")), 
        c(names(rev_data), "reviewed", "comment", "reviewer", "timestamp", "status")
      )
      rev_data_remote <- dplyr::collect(dplyr::tbl(con, "all_review_data"))
      expect_true(is.character(rev_data_remote[["timestamp"]])) 
      rev_data_remote[["timestamp"]] <- NULL
      expect_equal(
        rev_data_remote,
        dplyr::as_tibble(cbind(rev_data, review_expected))
      )
      
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "db_synch_time")), 
        dplyr::tibble(synch_time = "")
      )
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "query_data")), 
        dplyr::as_tibble(query_data_skeleton)
      )
    })
  }
)


describe(
  "db_update(). Feature 1 | As a user, I want to be able to update the database.", 
  {
    comvars = c("key_col1", "item_group")
    old_data <- data.frame(
      key_col1  = "Test_name",
      item_group = "Visit 1",
      item_name = "Test_item",
      event_date = "2023-11-01",
      edit_date_time = "2023-11-05 01:26:00"
      )
    new_data <- data.frame(
      key_col1 = "9999",
      item_group = "ZZ", 
      item_name = "new_test_item",
      event_date    = "1950/01/01", 
      edit_date_time = "2023-11-12 01:01:01"
    ) 
    review_cols <- data.frame(
      reviewed = "Yes",
      comment = "", 
      reviewer = "Admin",
      timestamp = "2023-11-13 01:01:01", 
      status = "old"
    )
    it("Never changes original entries in the database", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      
      DBI::dbWriteTable(con, "all_review_data", cbind(old_data, review_cols))
      df_old <- dplyr::as_tibble(cbind(old_data, review_cols))
      
      db_update(rbind(old_data, new_data), db_path = temp_path, 
                common_vars = comvars)
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "all_review_data"))[1, ], 
        df_old
        )
    })
    it("Adds a new row to a database if there are new rows", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      DBI::dbWriteTable(con, "all_review_data", cbind(old_data, review_cols))
      
      db_update(rbind(old_data, new_data), db_path = temp_path, 
                common_vars = comvars)
      expect_snapshot(dplyr::collect(dplyr::tbl(con, "all_review_data")))
    })
    it("Adds a new row for each data point with a new/updated EditdateTime.", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      DBI::dbWriteTable(con, "all_review_data", cbind(old_data, review_cols))
      updated_row_data <- old_data |> 
        dplyr::mutate(edit_date_time = "2023-11-13 01:01:01")
      
      db_update(updated_row_data, db_path = temp_path, common_vars = comvars)
      expect_snapshot(print(dplyr::collect(dplyr::tbl(con, "all_review_data")), 
                            width = 10))
    })
    it("Does not change the database if there are no changes (latest max edit_date_time is the same)", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite") 
      con <- get_db_connection(temp_path)
      DBI::dbWriteTable(con, "all_review_data", cbind(old_data, review_cols))
      
      expect_snapshot({
        db_update(old_data, db_path = temp_path, common_vars = comvars)
        dplyr::collect(dplyr::tbl(con, "all_review_data"))
      })
    })
    
  }
)

describe(
  "db_save_review() works", 
  {
    # # for dev purposes:      
    # rv_row <- dplyr::tbl(db, "all_review_data") |>
    #   dplyr::filter(reviewed == "No") |>
    #   dplyr::filter(subject_id == "IME-AU01-001", item_group == "Adverse events") |>
    #   dplyr::collect()
    df <- data.frame(
      key_col1 = "9999",
      item_group = "Group 1", 
      item_name = "Test item",
      event_date    = "1950/01/01", 
      edit_date_time = "2023-11-12 01:01:01"
    )
    
    old_review <- data.frame( 
      reviewed = "No",
      comment = "", 
      reviewer = "",
      timestamp = "2023-11-14 01:01:01", 
      status = "new"
    ) 
    
    new_review <- data.frame(
      reviewed = "Yes",
      comment = "Succes", 
      reviewer = "Admin 01",
      timestamp = "2023-11-14 01:01:01", 
      status = "old"
    )
    
    it("Can update a review info row", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      con <- get_db_connection(temp_path)
      DBI::dbWriteTable(con, "all_review_data", cbind(df, old_review))
      
      db_save_review(
        cbind(df, new_review), 
        temp_path, 
        tables = c("all_review_data"),
        common_vars = c("key_col1", "item_group", "item_name"), 
        review_by = c("key_col1", "item_group")
      )
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "all_review_data")), 
        dplyr::as_tibble(rbind(cbind(df, old_review), cbind(df, new_review))
        )
      )
    })
    it("Updates all items from a item_group as reviewed", {
      df <- data.frame(
        key_col1 = "9999",
        item_group = "Group 1", 
        item_name = c("Test item", "Test item2"),
        event_date    = c("1950/01/01", "1960/01/01"), 
        edit_date_time = c("2023-11-12 01:01:01", "2023-11-10 01:01:01")
      )
      old_review <- data.frame( 
        reviewed = "No",
        comment = "", 
        reviewer = "",
        timestamp = "", 
        status = "new"
      ) 
      new_review <- data.frame(
        reviewed = "Yes",
        comment = "Succes", 
        reviewer = "Admin 01",
        timestamp = "2023-11-14 01:01:01", 
        status = "old"
      )
      review_row <- data.frame(
        key_col1 = "9999",
        item_group = "Group 1"
      ) |> 
        cbind(new_review)
      
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      con <- get_db_connection(temp_path)
      DBI::dbWriteTable(con, "all_review_data", cbind(df, old_review))
      db_save_review(
        review_row, 
        temp_path, 
        tables = c("all_review_data"),
        common_vars = c("key_col1", "item_group", "item_name"), 
        review_by = c("key_col1", "item_group")
      )
      expect_true(is.data.frame(dplyr::collect(dplyr::tbl(con, "all_review_data"))))
      results <- dplyr::collect(dplyr::tbl(con, "all_review_data"))
      expect_equal(nrow(results), 4)
      expect_equal(results$status, c("new", "new", "old", "old"))
      expect_equal(
        results,
        dplyr::as_tibble(rbind(cbind(df, old_review), cbind(df, new_review)))
      )
    })
    
    
    
    it("warns with multiple rows in the review_row object", {
      temp_path <- withr::local_tempfile(fileext = ".sqlite")
      con <- get_db_connection(temp_path)
      
      DBI::dbWriteTable(con, "all_review_data", cbind(df, old_review))
      db_save_review(
        rbind(cbind(df, new_review), cbind(df, new_review)), 
        temp_path, 
        tables = "all_review_data",
        common_vars = c("key_col1", "item_group", "item_name"), 
        review_by = c("key_col1", "item_group")
      ) |> expect_warning()
    })
  }
)

describe(
  "db_save() works", 
  {
    temp_path <- withr::local_tempfile(fileext = ".sqlite") 
    con <- get_db_connection(temp_path)
    DBI::dbWriteTable(con, "query_data", query_data_skeleton)
    
    it("can save a query", {
      new_query <- data.frame(
        query_id = "ID124234", 
        type = "Normal",
        subject_id = "ID1",
        event_label = "Visit 1",
        item_group = "Vital signs",
        item = "Heart rate",
        timestamp = "2023-11-01 01:01:01", 
        n = 1, 
        reviewer = "Admin01", 
        query = "Test query",
        resolved = "",
        resolved_date = "", 
        edit_reason = ""
      )
      db_save(data = new_query, db_path = temp_path, db_table = "query_data")
      expect_equal(
        dplyr::collect(dplyr::tbl(con, "query_data")), 
        dplyr::as_tibble(new_query)
        )
    })
  }
)


describe("db_get_query can collect latest query data from a database", {
  temp_path <- withr::local_tempfile(fileext = ".sqlite") 
  con <- get_db_connection(temp_path)
  
  new_query <- dplyr::tibble(
    query_id = "ID124234", 
    subject_id = "ID1",
    n = 1:2,
    timestamp = c("2024-02-05 01:01:01", "2024-04-01 01:01:01"),
    other_info = "testinfo"
    ) 
  DBI::dbWriteTable(con, "query_data", new_query)
  
  it("Can collect the desired data.", {
    query_output <- db_get_query(temp_path, query_id = "ID124234", n = 1)
    expect_equal(query_output, new_query[1, ])
  })
  
  it("Collects an empty data frame if query_id or n are not found", {
    query_output <- db_get_query(temp_path, query_id = "non-existent", n = 1)
    expect_equal(query_output, new_query[0,])
    query_output <- db_get_query(temp_path, query_id = "ID124234", n = 6)
    expect_equal(query_output, new_query[0,])
  })
  
  it("Collects all rows with the same query ID if n is set to NULL", {
    query_output <- db_get_query(temp_path, query_id = "ID124234", n = NULL)
    expect_equal(query_output, new_query)
  })
  
})


describe("db_get_review can collect latest review data from a database", {
  temp_path <- withr::local_tempfile(fileext = ".sqlite") 
  con <- get_db_connection(temp_path)
 
  review_data <- data.frame(
    subject_id = "Test_name",
    event_name = "Visit 1",
    item_group = "Test_group",
    form_repeat = 1,
    item_name = "Test_item",
    edit_date_time = "2023-11-05 01:26:00",
    timestamp = "2024-02-05 01:01:01"
  ) |> 
    dplyr::as_tibble()
  DBI::dbWriteTable(con, "all_review_data", review_data)

  it("Can collect the desired data.", {
    output <- db_get_review(temp_path, subject = "Test_name", form = "Test_group")
    expect_equal(output, review_data)
  })
  
  it("Collects an empty data frame if the requested subject or form are not found", {
    output <- db_get_review(temp_path, subject = "Non-existent", 
                                         form = "Test_group")
    expect_equal(output, review_data[0,])
    output <- db_get_review(temp_path, subject = "Test_name", 
                                        form = "Non-existent")
    expect_equal(output, review_data[0,])
  })

})
