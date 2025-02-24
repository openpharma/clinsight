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
  
  it("Creates the same tables as create_table, but with review information included", {
    appdata <- get_appdata(clinsightful_data)
    table_names <- setNames(nm = names(appdata))
    form_items <- get_meta_vars(appdata)$items
    rev_data <- lapply(table_names, \(x){
      appdata[[x]] |> 
        get_review_data() |> 
        dplyr::mutate(
          id = dplyr::row_number(),
          reviewed = rep_len(c("Yes", "No"), length.out = dplyr::n()),
          status = ifelse(reviewed == "No", "old", "new")
        )  
    })
    # ignore AE/SAE column adjustments:
    testthat::local_mocked_bindings(
      adjust_ae_form_table = function(data, is_SAE){data}
    )
    output <- lapply(table_names, \(x){
      get_form_table(
        form_data = appdata[[x]], 
        form_review_data = rev_data[[x]], 
        form = x,
        form_items = form_items[[x]],
        active_subject = "BEL_04_772", 
        is_reviewed = TRUE
      )
    })
    lapply(table_names, \(x){
      cat("get_form_table | Verifying review data in form '", x, "'\n")
      expect_true(is.data.frame(output[[!!x]]))
      expect(
        "o_reviewed" %in% names(output[[x]]), 
        paste0("`o_reviewed` is an expected column for data in form `", x, "`.")
      )
      enabled_rows <- lapply(output[[x]][["o_reviewed"]], \(i) isFALSE(i$disabled)) |> unlist()
      if(any(enabled_rows)){
        expect_equal(unique(output[[!!x]][enabled_rows,]$subject_id), "BEL_04_772")
      } else{
        expect_true(!"BEL_04_772" %in% unique(output[[!!x]]$subject_id))
      }
    }) |> 
      invisible()
    
    output_tables <- lapply(table_names, \(x){
      output[[x]] |> 
        dplyr::select(-o_reviewed) |> 
        lapply(\(x){if(is.character(x)) gsub("\\**<\\/*b>", "", x) else x}) |> 
        dplyr::as_tibble() 
    })
    original_tables <- lapply(table_names, \(x){
      create_table(appdata[[x]], expected_columns = names(form_items[[x]])) 
    })
    lapply(table_names, \(x){
      expect_equal(original_tables[[!!x]], output_tables[[!!x]])
    })
  })
})

describe("adjust_ae_form_table() works as expected", {
  
})
