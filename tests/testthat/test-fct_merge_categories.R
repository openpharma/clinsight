describe("merge_item_pair() works", {
  it("merges two items by combining them as expected", {
    df <- data.frame(
      subject_id = 1,
      event = 1,
      item_name = c("Administered", "Administered_OTHER"),
      item_value = c("No", "Medication expired")
    )
    expect_no_error({
      output <- merge_item_pair(
        df, 
        item_name = "Administered", 
        item_name_other =  "Administered_OTHER", 
        id_cols = c("subject_id", "event")
      )
    })
    expected_output <- data.frame(
      subject_id = 1, event = 1, item_name = "Administered", 
      item_value = "No (Medication expired)"
    )
    expect_equal(expected_output, output)
    
  })
  it("can use the option merge_action to replace the 
     item with the other one if available", {
    df <- data.frame(
      subject_id = 1,
      event = 1,
      item_name = c("Administered", "Administered_OTHER"),
      item_value = c("No", "Medication expired")
    )
    expect_no_error({
      output <- merge_item_pair(
        data = df, 
        item_name = "Administered", 
        item_name_other =  "Administered_OTHER", 
        id_cols = c("subject_id", "event"), 
        merge_action = "replace"
      )
    })
    expected_output <- data.frame(
      subject_id = 1, event = 1, item_name = "Administered", 
      item_value = "Medication expired"
    )
    expect_equal(expected_output, output)
  })
  it("errors with incorrect or unexpected input", {
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    args <- list(
      data = df, 
      item_name = "Administered", 
      item_name_other =  "Administered_OTHER", 
      id_cols = c("subject_id", "event"), 
      merge_action = "replace"
    )
    change_args <- function(x){modifyList(x = args, val = x)}
    expect_error(
      do.call("merge_item_pair", change_args(list(data = "incorrect")))
      )
    expect_error(
      do.call("merge_item_pair", change_args(list(item_name = mtcars)))
    )
    expect_error(
      do.call("merge_item_pair", change_args(list(item_name = NA_character_))),
      "item_name cannot be missing"
    )
    expect_error(
      do.call("merge_item_pair", change_args(list(item_name_other = NA_character_))),
      "item_name_other cannot be missing"
    )
    expect_error(
      do.call("merge_item_pair", change_args(list(item_name_other = mtcars)))
    )
    expect_error(
      do.call("merge_item_pair", change_args(list(item_name_other = "Administered"))),
      "item_name cannot be the same as item_name_other"
    )
    expect_error(
      do.call("merge_item_pair", change_args(list(id_cols = mtcars)))
    )
    expect_error(
      do.call("merge_item_pair", change_args(list(merge_action = TRUE)))
    )
    expect_error(
      do.call("merge_item_pair", change_args(list(merge_action = "test")))
    )
  })
  it("returns item_name_other if item_name is missing while 
     not changing other item_names with the same id_cols", {
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    expected_outcome <- data.frame(
      subject_id = 1,
      event = c(1,2,1,2),
      item_name = c("Administered", "Administered", "non-existing", "non-existing"),
      item_value = c("Yes", "No", "", "Medication expired")
    )
    expect_equal(
      merge_item_pair(
        data = df, 
        item_name = "non-existing", 
        item_name_other = "Administered_OTHER", 
        id_cols = c("subject_id", "event"), 
        merge_action = "combine"
        ),
      expected_outcome
    )
    expect_equal(
      merge_item_pair(
        data = df, 
        item_name = "non-existing", 
        item_name_other = "Administered_OTHER", 
        id_cols = c("subject_id", "event"), 
        merge_action = "replace"
      ),
      expected_outcome
    )
  })
  it("returns value of item_name if item_name_other is missing", {
    # test that we can either combine or replace item_name with the item_name_other value:
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    expect_no_error({
      output <- merge_item_pair(
        data = df, 
        item_name = "Administered", 
        item_name_other =  "Administered_OTHER", 
        id_cols = c("subject_id", "event")
      )
    })
    expected_output <- data.frame(
      subject_id = 1, event = 1:2, item_name = "Administered", 
      item_value = c("Yes", "No (Medication expired)")
    )
    expect_equal(expected_output, output)
    
    expect_equal(
      merge_item_pair(
        data = df, 
        item_name = "Administered", 
        item_name_other =  "Administered_OTHER", 
        id_cols = c("subject_id", "event")
      ),
      expected_output
    )
  })
  it("returns original data frame if item_name_other is not found in the data", {
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    expect_equal(
      merge_item_pair(
        data = df, 
        item_name = "Administered", 
        item_name_other =  "Administered_OTHER_NOT_FOUND", 
        id_cols = c("subject_id", "event")
      ),
      df
    )
  })
  it("warns if all id_cols and the item_name column do not uniquely identify a row
     and returns the original dataset", {
    # test that warns if all id_cols and the item_name column do not uniquely identify a row
    # and returns the original dataset:
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    expect_warning(
      {
        output <- merge_item_pair(
          data = df, 
          item_name = "Administered", 
          item_name_other =  "Administered_OTHER", 
          id_cols = c("subject_id")
        )
      }, 
      " do not uniquely identify the rows. Cannot merge 'Administered' with 'Administered_OTHER'."
    )
    expect_equal(df, output)
  })
  it("uses the latest edit_date_time if the edit_date_time columns is available", {
    # test that it uses the latest edit_date_time if the edit_date_time columns is available:
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired"),
      edit_date_time = as.POSIXct(c("2021-01-01 12:00:00", "2021-10-10 12:00:00", 
                                    "2021-01-01 12:00:00", "2021-11-01 12:00:00"))
    )
    expected_output <- data.frame(
      subject_id = 1, event = 1:2, item_name = "Administered", 
      item_value = c("Yes", "No (Medication expired)"),
      edit_date_time = as.POSIXct(c("2021-10-10 12:00:00", "2021-11-01 12:00:00"))
    )
    expect_equal(
      merge_item_pair(
        data = df, 
        item_name = "Administered", 
        item_name_other =  "Administered_OTHER", 
        id_cols = c("subject_id", "event")
      ),
      expected_output
    )
  })
})

describe("merge_item_pairs_by_suffix works", {
  it("can merge item pairs based on suffix matching", {
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    expect_no_error({
      output <- merge_item_pairs_by_suffix(
        data = df, 
        suffix = "_OTHER", 
        id_cols = c("subject_id", "event")
      )
    })
    expected_output <- data.frame(
      subject_id = 1, event = 1:2, item_name = "Administered", 
      item_value = c("Yes", "No (Medication expired)")
    )
    expect_equal(expected_output, output)
  })
  it("returns original data if no suffix was defined", {
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    expect_output(
      output <- merge_item_pairs_by_suffix(
        data = df, 
        suffix = "", 
        id_cols = c("subject_id", "event")
      ),
      "No suffix defined. Skipping merging of items."
    )
    expect_equal(df, output)
    expect_output(
      output <- merge_item_pairs_by_suffix(
        data = df, 
        suffix = NULL, 
        id_cols = c("subject_id", "event")
      ),
      "No suffix defined. Skipping merging of items."
    )
    expect_equal(df, output)
  })
  it("errors with incorrect input", {
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    args <- list(
      data = df, 
      suffix = "_OTHER", 
      id_cols = c("subject_id", "event")
    )
    change_args <- function(x){modifyList(x = args, val = x)}
    expect_error(
      do.call("merge_item_pairs_by_suffix", change_args(list(data = "incorrect")))
    )
    expect_error(
      do.call("merge_item_pairs_by_suffix", change_args(list(suffix = mtcars)))
    )
    expect_error(
      do.call("merge_item_pairs_by_suffix", change_args(list(id_cols = mtcars)))
    )
  })
  it("returns original data frame if suffix does not match any variables", {
    df <- data.frame(
      subject_id = 1,
      event = c(1,1,2,2),
      item_name = rep(c("Administered", "Administered_OTHER"), times = 2),
      item_value = c("Yes", NA_character_, "No", "Medication expired")
    )
    expect_output(
      output <- merge_item_pairs_by_suffix(
        data = df, 
        suffix = "_NOT_FOUND", 
        id_cols = c("subject_id", "event")
      ),
      "No variable pairs found for merging. Returning data."
    )
    expect_equal(df, output)
  })
})
