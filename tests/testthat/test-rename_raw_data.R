describe("rename_raw_data() renames raw study data and throws informative errors if needed.", {
  
  it("renames a data frame as expected", {
    specs <- metadata$column_specs
    specs$name_raw <- names(mtcars)
    
    expect_no_error(rename_raw_data(mtcars, column_specs = specs))
    outcome <- rename_raw_data(mtcars, column_specs = specs)
    expect_true(is.data.frame(outcome))
    expect_equal(names(outcome), specs$name_new)
  })
  
  it("errors with incorrect input", {
    specs <- metadata$column_specs
    specs$name_raw <- names(mtcars)
    expect_error(
      rename_raw_data(mtcars, column_specs = "incorrect input"),
      "should be a data frame"
    )
    expect_error(
      rename_raw_data("incorrect input", column_specs = specs), 
      "should be a data frame"
    )
    incorrect_raw_names <- specs
    incorrect_raw_names$name_raw <- paste0("incorrect_name_", 1:nrow(specs))
    expect_error(
      rename_raw_data(mtcars, column_specs = incorrect_raw_names), 
      "The following columns are missing"
    )
    incorrect_new_names <- specs
    incorrect_new_names$name_new <- paste0("incorrect_name_", 1:nrow(specs))
    expect_error(
      rename_raw_data(mtcars, column_specs = incorrect_new_names), 
      "The following columns are missing"
    )
  })
})
