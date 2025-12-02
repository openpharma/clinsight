describe("count_adverse_events works", {
  AE_data <- data.frame(
    "subject_id" = "Subj01", 
    "event_name" = "Screening",
    "item_group" = "Adverse events",
    "form_repeat" = 1,
    "item_name" = "Serious Adverse Event",
    "item_value" = "No"
  )
  it("Gives the expected output", {
    expect_equal(
      count_adverse_events(AE_data),
      data.frame("subject_id" = "Subj01", AEs = 1, SAEs = 0)
    )
  })
  it("expands the table with expected ids if they do not occur in the AE table", {
    expected_outcome <- data.frame(
      "subject_id" = c("Subj01", "Subj02"),
      AEs = c(1, 0),
      SAEs = c(0,0)
      )
    expect_equal(
      count_adverse_events(AE_data, all_ids = c("Subj01", "Subj02")),
      expected_outcome
    )
  })
  it("Warns if item 'Serious Adverse Event' was not found and returns a data 
     frame with question marks as AEs and SAEs", 
     {
       AE_data <- data.frame(
         "subject_id" = "Subj01", 
         "event_name" = "Screening",
         "item_group" = "Adverse events",
         "form_repeat" = 1,
         "item_name" = "xxx",
         "item_value" = "No"
       )
       expect_warning(
         df <- count_adverse_events(AE_data),
         "item 'Serious Adverse Event' not found"
       )
       expect_equal(df, data.frame(subject_id = "Subj01", AEs = "?", "SAEs" = "?"))
     }
  )
  it("provides an empty data frame if data contains zero rows", {
    expect_equal(
      count_adverse_events(data.frame()),
      data.frame(subject = character(), AEs = numeric(), SAEs = numeric())
    )
  })
  it("errors if any of the key_columns or the item_value column are missing", {
    AE_data <- data.frame(
      "subject_id" = "Subj01", 
      "event_name" = "Screening",
      "item_group" = "Adverse events",
      "form_repeat" = 1,
      "item_name" = "xxx",
      "item_value" = "No"
    )
    expect_error(
      count_adverse_events(dplyr::select(AE_data, -subject_id)),
      "One or more required columns are missing"
    )
    expect_error(
      count_adverse_events(dplyr::select(AE_data, -event_name)),
      "One or more required columns are missing"
    )
    expect_error(
      count_adverse_events(dplyr::select(AE_data, -item_group)),
      "One or more required columns are missing"
    )
    expect_error(
      count_adverse_events(dplyr::select(AE_data, -form_repeat)),
      "One or more required columns are missing"
    )
    expect_error(
      count_adverse_events(dplyr::select(AE_data, -item_name)),
      "One or more required columns are missing"
    )
    expect_error(
      count_adverse_events(dplyr::select(AE_data, -item_value)),
      "One or more required columns are missing"
    )
  })
})
