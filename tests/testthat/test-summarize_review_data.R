describe(
  "summarize_review_data() works", 
  {
    set.seed(2023)
    review_df <- get_review_data(clinsightful_data[1:3000, ]) |> 
      dplyr::mutate(
        status = sample(c("old", "new", "updated"), dplyr::n(), replace = TRUE), 
                    reviewed = "No") |> 
      dplyr::select(
        subject_id, 
        "Form" = item_group, 
        "Event" = event_name, 
        `Edit date` = edit_date_time, 
        status, 
        reviewed
      )
    
    it("Scenario 1 - Given a random data set provided, I expect that summary dataframe snapshot will be as expected", {
      expect_snapshot(
        summarize_review_data(
          review_df, 
          common_vars = c("subject_id", "Form"), 
          event_var = "Event", 
          date_time_vars = "Edit date"
        )
      )
    })   
    it(
      "Scenario 1 - Given that none of the data within a group was reviewed, 
          and all values in the [status] column are 'new',
          I expect that, in the summarized data frame, 
          all values in the [reviewed] column are 'No' 
          and all values in the [status] column labels are 'new'.", 
      {
        review_df <- data.frame(
          subject_id = "Test_name",
          item_group = "Test_group",
          event_name = c("Visit 1", "Visit 2"),
          edit_date_time = "2023-11-05 01:26:00",
          status = "new",
          reviewed = "No"
        )
        expected_df <- review_df[1, ]
        expected_df$edit_date_time <- expected_df$edit_date_time
        expected_df$event_name <- c("Visit 1, Visit 2")
        expect_equal(
          summarize_review_data(review_df),
          expected_df
        )
      }
    )
    it(
      "Scenario 2 - Given that all data within a group was reviewed,
          I expect that, in the summarized data frame, 
          all values in the [reviewed] column are 'Yes' 
          and all values in the [status] column labels are 'old'.", 
      {
        review_df <- data.frame(
          subject_id = "Test_name",
          item_group = "Test_group",
          event_name = c("Visit 1", "Visit 2"),
          edit_date_time = "2023-11-05 01:26:00",
          status = "new",
          reviewed = "No"
        )
        expected_df <- review_df[1, ]
        expected_df$edit_date_time <- expected_df$edit_date_time
        expected_df$event_name <- c("Visit 1, Visit 2")
        expect_equal(
          summarize_review_data(review_df),
          expected_df
        )
      }
    )
    it(
      "Scenario 3 - Given that some data within a group was new and some was updated,
          I expect that, in the summarized data frame, 
          all values in the [reviewed] column are 'No' 
          and all values in the [status] column labels are 'new/updated'.", 
      {
        review_df <- data.frame(
          subject_id = "Test_name",
          item_group = "Test_group",
          event_name = c("Visit 1", "Visit 2"),
          edit_date_time = "2023-11-05 01:26:00",
          status = c("updated", "new"),
          reviewed = "No"
        )
        expected_df <- review_df[1, ]
        expected_df$edit_date_time <- expected_df$edit_date_time
        expected_df$event_name <- c("Visit 1, Visit 2")
        expected_df$status <- c("new/updated")
        
        expect_equal(
          summarize_review_data(review_df),
          expected_df
        )
      }
    )
    it(
      "Scenario 4 - Given that some data within a group was old and some was new,
          I expect that, in the summarized data frame, 
          all values in the [reviewed] column are 'No' 
          and all values in the [status] column labels are 'new'.", 
      {
        review_df <- data.frame(
          subject_id = "Test_name",
          item_group = "Test_group",
          event_name = c("Visit 1", "Visit 2"),
          edit_date_time = c("2023-11-05 01:26:00", "2023-11-09 01:01:01"),
          status = c("old", "new"),
          reviewed = c("Yes", "No")
        )
        expected_df <- review_df[1, ]
        expected_df$edit_date_time <- review_df$edit_date_time[2]
        expected_df$event_name <- c("Visit 1, Visit 2")
        expected_df$status <- "new"
        expected_df$reviewed <- "No"
        
        expect_equal(
          summarize_review_data(review_df),
          expected_df
        )
      }
    )
    it(
      "Scenario 5 - Given that some values within a group were old and some were updated,
          I expect that, in the summarized data frame, 
          all values in the [reviewed] column are 'No' 
          and all values in the [status] column labels are 'updated'.", 
      {
        review_df <- data.frame(
          subject_id = "Test_name",
          item_group = "Test_group",
          event_name = c("Visit 1", "Visit 2"),
          edit_date_time = "2023-11-05 01:26:00",
          status = c("old", "updated"),
          reviewed = c("Yes", "No")
        )
        expected_df <- review_df[1, ] |> 
          dplyr::as_tibble()
        expected_df$edit_date_time <- expected_df$edit_date_time
        expected_df$event_name <- c("Visit 1, Visit 2")
        expected_df$status <- c("updated")
        expected_df$reviewed <- c("No")
        
        expect_equal(
          dplyr::as_tibble(summarize_review_data(review_df)),
          expected_df
        )
      }
    )
    it(
      "Scenario 6 - Given that some values within a group were 'old', some 'updated' and some 'new',
          I expect that, in the summarized data frame, 
          all values in the [reviewed] column are 'No' 
          and all values in the [status] column labels are 'new/updated'.", 
      {
        review_df <- data.frame(
          subject_id = "Test_name", 
          item_group = "Test_group",
          event_name = c("Visit 1", "Visit 2", "Visit 3"),
          edit_date_time = "2023-11-05 01:26:00",
          status = c("old", "updated", "new"),
          reviewed = c("Yes", "No", "No")
        )
        expected_df <- review_df[1, ] |> 
          dplyr::as_tibble()
        expected_df$edit_date_time <- expected_df$edit_date_time
        expected_df$event_name <- c("Visit 1, Visit 2, Visit 3")
        expected_df$status <- "new/updated"
        expected_df$reviewed <- "No"
        
        expect_equal(
          dplyr::as_tibble(summarize_review_data(review_df)),
          expected_df
        )
      }
    )
  }
)
