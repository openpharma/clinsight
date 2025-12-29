describe("get_timeline_data works", {
  it("creates a data frame with timeline data with the expected columns", {
    appdata <- clinsightful_data |> 
      dplyr::filter(
        subject_id %in% c("BEL_08_885"),
        item_group %in% c("Adverse events",  "Vital signs", "General")
      ) |> 
      get_appdata()
    appvars <- get_meta_vars(appdata)
    available_data <- get_available_data(appdata)
    output <- get_timeline_data(appdata, available_data)
    expect_true(is.data.frame(output))
    expect_equal(
      names(output), 
      c("subject_id", "content", "form_repeat", "item_group", "start", "group", 
        "end", "title", "className", "id", "order")
    )
  })
  it("does not error with missing data", {
    appdata <- clinsightful_data |> 
      dplyr::filter(
        subject_id %in% "BEL_08_885",
        item_group %in% c("Adverse events",  "Vital signs", "General")
      ) |> 
      get_appdata()
    appvars <- get_meta_vars(appdata)
    available_data <- get_available_data(appdata)
    expected_columns <- c("subject_id", "content", "form_repeat", "item_group", 
                          "start", "group", "end", "title", "className", "id", "order")
    output <- get_timeline_data(appdata["Adverse events"], available_data)
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
    output <- get_timeline_data(appdata["Vital signs"], available_data)
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
    output <- get_timeline_data(appdata["General"], available_data)
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
    expect_warning(
      output <- get_timeline_data(appdata["Gener"], available_data),
      "No data found"
    )
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
    expect_warning(
      output <- get_timeline_data(appdata["Gener"]),
      "No data found"
    )
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    
  })
  it("gathers visit data from data frame if data frame available_data is not provided", {
    appdata <- clinsightful_data |> 
      dplyr::filter(
        subject_id %in% c("BEL_08_885"),
        item_group %in% c("Adverse events",  "Vital signs", "General")
      ) |> 
      get_appdata()
    expected_columns <- c("subject_id", "content", "form_repeat", "item_group", 
                          "start", "group", "end", "title", "className", "id", "order")
    output <- get_timeline_data(appdata["Vital signs"])
    expect_true(is.data.frame(output))
    expect_equal(names(output), expected_columns)
    expect_equal(output$content, c("Screening", "Visit 1"))
  })
  
})
