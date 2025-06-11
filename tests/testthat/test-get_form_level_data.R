describe("get_form_level_data() works", {
  it("adds default form-level columns to the input, sets the column types of 
     the default correctly, and keeps the column specs of the default values", {
       
    df <- data.frame("keycol" = "form2", "newvar" = "important")
    
    output <- get_form_level_data(
      df, 
      all_forms = c("form1", "form2"),
      form_column = "keycol"
    )
    expected_output <- data.frame(
      keycol = c("form1", "form2"),
      newvar = c(NA, "important"),
      form_level_defaults
    )
    expect_equal(output, expected_output)
    
  })
  
  it("only updates a value to the default one if the value is missing", {
    df <- data.frame(
      "keycol" = c("form2", "form1", "form3"),
      "newvar" = c("important", NA, NA),
      "review_required" = c("FALSE", "FALSE", NA)
      )
    output <- get_form_level_data(
      df, 
      all_forms = paste0("form", 1:3),
      form_column = "keycol"
    )
    expected_output <- data.frame(
      keycol = paste0("form", 1:3),
      newvar = c(NA, "important", NA),
      review_required = c(FALSE, FALSE, TRUE),
      item_scale = as.logical(NA),
      use_unscaled_limits = as.logical(NA)
    )
    expect_equal(output, expected_output)
  })
  
  it("warns if not valid data input is provided and returns defaults", {
    expected_output <- data.frame(fc = "f1", form_level_defaults)
    
    expect_warning(
      output <- get_form_level_data(list(), all_forms = "f1", form_column = "fc"),
      "No valid update table found. Falling back to defaults"
    )
    expect_equal(output, expected_output)
    
    expect_warning(
      output <- get_form_level_data(NULL, all_forms = "f1", form_column = "fc"),
      "No valid update table found. Falling back to defaults"
    )
    expect_equal(output, expected_output)
  })
  it("errors if the key column (form_column) is not available", {
    df <- data.frame("wrong_keycol" = "form2", "newvar" = "important")
    expect_error(
      get_form_level_data(df, all_forms = "form2", form_column = "keycol2"),
      "'keycol2' missing in 'form_level_data' table"
    )
  })
  it("warns if any defined forms do not match the expected ones in 'all_forms'", {
    df <- data.frame(
      "keycol" = c("undefined form1", "undefined form2", "form2"), 
      "newvar" = c("var_to_be_ignored1", "var_to_be_ignored2", "important")
      )
    expect_warning(
      output <- get_form_level_data(
        df, 
        all_forms = c("form1", "form2"),
        form_column = "keycol"
      ), 
      "Ignoring vars defined in 'form_level_data' but not in metadata"
      )
    expected_output <- data.frame(
      keycol = c("form1", "form2"),
      newvar = c(NA, "important"),
      form_level_defaults
    )
    expect_equal(output, expected_output)
  })
  it("warns if no forms with form level data are found, and returns defaults", {
    df <- data.frame("keycol" = character())#, "newvar" = character())
    expect_warning({
      output <- get_form_level_data(
        df, 
        all_forms = "form1",
        form_column = "keycol"
      )
    }, 
    "No forms with form-level data found"
    )
    expected_output <- data.frame(keycol = "form1") |> 
      cbind(form_level_defaults)
    expect_equal(output, expected_output)
  })
})
