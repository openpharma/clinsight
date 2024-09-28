describe(
  "get_review_data(). Feature 1 | Select correct data to review. 
      As a user, I want to be able to extract data from a data frame, 
      with only the required columns and, if there are duplicates in the rows, the 
      row with the latest edit date-time is selected. ", 
  {
    set.seed(2023)
    df <- data.frame(
      key_col1 = sample(1:256, 12, replace = F),
      item_group = sample(LETTERS, 12, replace = F),
      "edit_date_time" = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 12),
      "event_date"    = sample(seq(as.Date('1950/01/01'), as.Date('1990/01/01'), by="day"), 12)
    ) |> 
      # add a controlled max date:
      rbind(
        data.frame(
          key_col1 = c(9999, 9999),
          item_group = c("ZZ", "ZZ"),
          "edit_date_time" = as.Date(c("2023-10-30", "2023-09-30")),
          "event_date"    = c("1950/01/01", "1950/01/01")
          )
      )
    it("returns a data frame", {
      expect_true(is.data.frame(get_review_data(df, common_vars = c("key_col1", "item_group"))))
    })
    it("returns the expected number of columns and rows, and selects the row with latest 
       edit date-time if there is a duplicated row", {
      outcome <- get_review_data(df, common_vars = c("key_col1", "item_group"))
      expect_equal(nrow(outcome), 13)
      expect_equal(colnames(outcome), c("key_col1", "item_group",
                                        "event_date", "edit_date_time"))
      expect_true(nrow(with(outcome, outcome[key_col1 == 9999, ])) == 1)
      expect_true(with(outcome, edit_date_time[key_col1 == 9999]) == "2023-10-30")
    })
    
    
  }
)
