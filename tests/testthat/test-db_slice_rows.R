describe(
  "db_slice_rows. Feature 1 | As a user, I want to be able to select rows in a database or 
    data frame, that have the lastest timestamp per selection group. This way, 
    I can select (for example) the latest data point with the latest review activity.
    I want to be able to at least filter using two timestamp variables, so that the data
    can be filtered on the latest edit of the study site (as captured in the external EDC system), 
    as well as on the latest edit of the review data by the reviewer in the application.", 
  {
    it(
      "Scenario 1 - Given a data frame with review data with one date-time variable [review_dttm] 
        and grouping variables [ID] and [group], 
        I expect that I can slice the database with the output being a data frame,
        and with the output data frame containing the rows with the latest data point per group", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        rev_data <-  data.frame(
          ID          = c("885", "361", "885"),
          group      = c("Adverse events", "Vital signs", "Adverse events"),
          item_name   = c("AE Number", "Systolic blood pressure", "AE Number"),
          event      = c("Any visit", "Visit 5", "Any visit"), 
          item_value = c("10", "20", "15"),
          edit_dttm = c("2023-09-30 01:01:01", "2023-08-30 01:01:01", "2023-10-02 01:01:01"),
          review_dttm = c("2023-11-01 01:01:01", "2023-08-30 01:01:01", "2023-11-10 01:01:01"),
          reviewed = c("No", "Yes", "No")
        ) |> 
          dplyr::as_tibble() # for easier comparisons, since db_slice_rows returns a tibble
        db_temp_connect(temp_path, DBI::dbWriteTable(con, "rev_data", rev_data))
        outcome <- db_slice_rows(temp_path, "rev_data", "edit_dttm", group_vars = c("ID", "group"))
        expect_true(is.data.frame(outcome))
        expect_equal(outcome, rev_data[-1, ])
      }
    )
    it(
      "Scenario 2 - Given a data frame with review data with and two date-time variables, 
        that is stored in a database,
        I expect that I can slice the data frame based on both date-time variables,
        selecting the rows with the latest date-time values.", 
      {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        rev_data_two_timevars <-  data.frame(
          ID          = c("885", "361", "885", "885"),
          group      = c("Adverse events", "Vital signs", "Adverse events", "Adverse events"),
          item_name   = c("AE Number", "Systolic blood pressure", "AE Number", "AE Number"),
          event      = c("Any visit", "Visit 5", "Any visit", "Any visit"), 
          item_value = c("10", "20", "15", "15"),
          edit_dttm = c("2023-09-30 01:01:01", "2023-08-30 01:01:01", "2023-10-02 01:01:01", "2023-10-02 01:01:01"),
          review_dttm = c("2023-11-01 01:01:01", "2023-08-30 01:01:01", "2023-11-10 01:01:01", "2023-12-01 01:01:01"),
          reviewed = c("No", "Yes", "No", "Yes")
        ) |> 
          dplyr::as_tibble() # for easier comparisons, since db_slice_rows returns a tibble
        db_temp_connect(temp_path, {
          DBI::dbWriteTable(con, "rev_data_two_timevars", rev_data_two_timevars)
        })
        outcome <- db_slice_rows(
          data = temp_path, 
          db_table = "rev_data_two_timevars",
          slice_vars =  c("review_dttm", "edit_dttm"),
          group_vars = c("ID", "group")
        )
        expect_true(is.data.frame(outcome))
        expect_equal(outcome, rev_data_two_timevars[c(2,4), ])
      }
    )
    it(
      "Scenario 3 - Given a data frame with the same date time values in the same group, 
      and the data frame being saved in a database,
      I expect a warning if I try to slice the database,
      and that the last entry per group is selected, which is the most recent entry.", {
        temp_path <- withr::local_tempfile(fileext = ".sqlite")
        rev_data_duplicate <-  data.frame(
          ID          = "885",
          group      = "Adverse events", 
          item_name   = "AE Number",
          event      = "Any visit", 
          item_value = "10",
          edit_dttm = "2023-09-30 01:01:01", 
          review_dttm = "2023-11-01 01:01:01",
          comment = c("", "Comment"),
          reviewed = "Yes"
        ) |> 
          dplyr::as_tibble() 
        db_temp_connect(temp_path, {
          DBI::dbWriteTable(con, "rev_data_duplicate", rev_data_duplicate)
        })
        expect_warning(
          db_slice_rows(
            data = temp_path, 
            db_table = "rev_data_duplicate",
            slice_vars =  c("review_dttm", "edit_dttm"),
            group_vars = c("ID", "group")
          )
        )
        expect_equal(
          db_slice_rows(
            data = temp_path, 
            db_table = "rev_data_duplicate",
            slice_vars =  c("review_dttm", "edit_dttm"),
            group_vars = c("ID", "group")
          ) |> 
            suppressWarnings(),
          rev_data_duplicate[2, ]
        )
      })
  }
)
