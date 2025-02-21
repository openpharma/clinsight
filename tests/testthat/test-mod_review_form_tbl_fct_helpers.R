describe("get_form_table() works", {
  it(
    "creates a form table similar to the on created with create_table, but with 
    not yet reviewed data marked bold and with the o_reviewed object stored in it,
    in which only the rows for the active_subject set are set to disabled=FALSE", 
    {
    appdata <- clinsightful_data[clinsightful_data$item_group == "CBC regular", ] |> 
      get_appdata()
    meta_vars <- get_meta_vars(appdata)
    rev_data <- get_review_data(appdata[["CBC regular"]]) |> 
      dplyr::mutate(
        id = dplyr::row_number(),
        reviewed = rep_len(c("Yes", "No"), length.out = dplyr::n()),
        status = ifelse(reviewed == "No", "old", "new")
      )
    form_items <- meta_vars$items[["CBC regular"]]
    
    output <- get_form_table(
      form_data = appdata[["CBC regular"]], 
      form_review_data = rev_data, 
      form = "CBC regular",
      form_items = form_items,
      active_subject = "BEL_07_193", 
      is_reviewed = TRUE
      )
    expect_true(is.data.frame(output))
    expect("o_reviewed" %in% names(output), "`o_reviewed` is an expected column for form data")
    enabled_rows <- lapply(output[["o_reviewed"]], \(x) isFALSE(x$disabled)) |> unlist()
    expect_equal(unique(output[enabled_rows,]$subject_id), "BEL_07_193")
    compare_output <- dplyr::select(output, -o_reviewed)
    compare_original <- create_table(
      appdata[["CBC regular"]], 
      expected_columns = names(form_items)
      )
    expect_equal(names(compare_output), names(compare_original))
    expect_equal(nrow(compare_output), nrow(compare_original))
    expect_equal(compare_output$subject_id, compare_original$subject_id)
    expect_equal(compare_output$event_name, compare_original$event_name)
    # TODO: the test below fails due to a bug in the code!
    # If data is new, it gets bold tags. But if data is missing, and there is an 
    # explanation, the explanation will not be shown (instead, NA is shown)
    # output_bare <- output |> 
    #   dplyr::select(-o_reviewed) |> 
    #   lapply(\(x){gsub("\\**<\\/*b>", "", x)}) |> 
    #   dplyr::as_tibble()
    # original_table <- create_table(appdata[["CBC regular"]], expected_columns = names(form_items))
    #expect_equal(original_table, output_bare)
  })
  
  it("Removes columns related to SAEs from the Adverse events table if is_SAE is 
      FALSE, since they will be shown in a separate table", {
    appdata <- clinsightful_data[clinsightful_data$item_group == "Adverse events", ] |> 
      get_appdata()
    form_items <- get_meta_vars(appdata)$items[["Adverse events"]]
    rev_data <- get_review_data(appdata[["Adverse events"]]) |> 
      dplyr::mutate(
        id = dplyr::row_number(),
        reviewed = rep_len(c("Yes", "No"), length.out = dplyr::n()),
        status = ifelse(reviewed == "No", "old", "new")
      )
    
    output <- get_form_table(
      form_data = appdata[["Adverse events"]], 
      form_review_data = rev_data, 
      form = "Adverse events",
      form_items = form_items,
      active_subject = "BEL_04_772", 
      is_reviewed = TRUE
    )
    expect_true(is.data.frame(output))
    expect("o_reviewed" %in% names(output), "`o_reviewed` is an expected column for form data")
    enabled_rows <- lapply(output[["o_reviewed"]], \(x) isFALSE(x$disabled)) |> unlist()
    expect_equal(unique(output[enabled_rows,]$subject_id), "BEL_04_772")
    
    output_table <- output |> 
      dplyr::select(-o_reviewed) |> 
      lapply(\(x){if(is.character(x)) gsub("\\**<\\/*b>", "", x) else x}) |> 
      dplyr::as_tibble() 
    original_table <- appdata[["Adverse events"]] |> 
      create_table(expected_columns = names(form_items)) |> 
      dplyr::filter(!grepl("Yes", `Serious Adverse Event`)) |> 
      dplyr::select(-dplyr::starts_with("SAE"))
    expect_equal(original_table, output_table)
    
    hypotension_ae <- with(
      output, 
      Name[subject_id == "BEL_04_772" & grepl("hypotension", tolower(Name))]
    )
    expect_true(length(hypotension_ae) != 0 )
    expect_true(grepl("<b>", hypotension_ae))
  })
  
  it("Shows SAE-specific columns if is_SAE is TRUE and form is 'Adverse events'", {
        appdata <- clinsightful_data[clinsightful_data$item_group == "Adverse events", ] |> 
          get_appdata()
        meta_vars <- get_meta_vars(appdata)
        rev_data <- get_review_data(appdata[["Adverse events"]]) |> 
          dplyr::mutate(
            id = dplyr::row_number(),
            reviewed = rep_len(c("Yes", "No"), length.out = dplyr::n()),
            status = ifelse(reviewed == "No", "old", "new")
          )
        form_items <- meta_vars$items[["Adverse events"]]
        
        output <- get_form_table(
          form_data = appdata[["Adverse events"]], 
          form_review_data = rev_data, 
          form = "Adverse events",
          form_items = form_items,
          active_subject = "DEU_02_482", 
          is_reviewed = TRUE,
          is_SAE = TRUE
        )
        expect_true(is.data.frame(output))
        expect("o_reviewed" %in% names(output), "`o_reviewed` is an expected column for form data")
        enabled_rows <- lapply(output[["o_reviewed"]], \(x) isFALSE(x$disabled)) |> unlist()
        expect_equal(unique(output[enabled_rows,]$subject_id), "DEU_02_482")
        
        output_table <- output |> 
          dplyr::select(-o_reviewed) |> 
          lapply(\(x){if(is.character(x)) gsub("\\**<\\/*b>", "", x) else x}) |> 
          dplyr::as_tibble() 
        original_table <- appdata[["Adverse events"]] |> 
          create_table(expected_columns = names(form_items)) |> 
          dplyr::filter(grepl("Yes", `Serious Adverse Event`)) |> 
          dplyr::select(dplyr::any_of(
            c("o_reviewed", "subject_id","form_repeat", "Name", "AESI",  "SAE Start date",
              "SAE End date", "CTCAE severity", "Treatment related",
              "Treatment action", "Other action", "SAE Category",
              "SAE Awareness date", "SAE Date of death", "SAE Death reason")
          )) |>
          adjust_colnames("^SAE ")
        expect_equal(original_table, output_table)
      })
  
})
