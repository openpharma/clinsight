describe(
  "update_review_data()  works", 
  {
    set.seed(2023)
    old_review_data <- data.frame(
      "key_col1" = sample(1:256, 12, replace = F),
      "item_group" = sample(LETTERS, 12, replace = F),
      "edit_date_time" = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 12),
      "event_date"    = sample(seq(as.Date('1950/01/01'), as.Date('1990/01/01'), by="day"), 12),
      "reviewed" = "Yes"
    ) |> 
      # add a controlled max date:
      rbind(
        data.frame(
          "key_col1" = 9999,
          "item_group" = "ZZ",
          "edit_date_time" = as.Date("2023-10-30"),
          "event_date"    = "1950/01/01",
          "reviewed" = "Yes"
        )
      )
    
    updated_review_data <- old_review_data |> 
      dplyr::rows_update(
        data.frame("key_col1" = c(9999), "edit_date_time" = as.Date("2023-11-05"), "reviewed" = NA_character_), 
        by = "key_col1"
        ) |> 
      rbind(
        data.frame(
          "key_col1" = 10000,
          "item_group" = "ZZ",
          "edit_date_time" = as.Date("2023-11-09"),
          "event_date"    = "1950/01/01",
          "reviewed" = NA_character_
        )
      ) 
    
    it("creates a data frame with the expected result", {
      result <- update_review_data(
        old_review_data, updated_review_data, 
        common_vars = c("key_col1", "item_group")
      )
      expected <- data.frame(
        key_col1 = c(9999, 10000),
        item_group = "ZZ",
        edit_date_time = as.Date(c("2023-11-05", "2023-11-09")),
        reviewed = "No",
        status = c("updated", "new")
      )
      expect_true(is.data.frame(result))
      expect_equal(dplyr::select(result, dplyr::all_of(colnames(expected))), expected)
    })
    
    it("warns if the updated dataset does not contain 
       any new data, and returns an empty data frame", {
      expect_warning(update_review_data(old_review_data, old_review_data, 
                         common_vars = c("key_col1", "item_group")))
      expect_true(
        nrow(suppressWarnings(update_review_data(old_review_data, old_review_data, 
                             common_vars = c("key_col1", "item_group")))) == 0
        )
    })
    it("warns if rows are not found in the updated dataset but still returns a 
    valid data frame; might happen if entries are deleted from the source", {
         missing_row_data <- updated_review_data |> dplyr::filter(key_col1 != 9999) 
         expect_warning(update_review_data(old_review_data, missing_row_data, 
                                           common_vars = c("key_col1", "item_group")))
         expect_true(is.data.frame(suppressWarnings(
           update_review_data(old_review_data, missing_row_data, 
                              common_vars = c("key_col1", "item_group"))
           )))
       })
  }
)
