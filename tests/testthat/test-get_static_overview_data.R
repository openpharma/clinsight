describe(
  "get_static_overview_data() works. ", 
  {
    appdata <- get_appdata(clinsightful_data)
    vars <- get_meta_vars(appdata)
    all_forms <- data.frame(
      main_tab = c(rep("Common events", times = 4), rep("Study data", times = 5)),
      form = c("Adverse events", "Conc. Procedures", "Medical History", "Medication", 
               "CBC regular",  "Electrolytes", "Liver function", 
               "Renal function", "Vital signs")
    )
    apptables <- lapply(
      setNames(names(appdata), names(appdata)), \(x){
        create_table(appdata[[x]], expected_columns = names(vars$items[[x]]))
      })
    
    it("creates the a data frame with the expected column names", {
      test_results <- get_static_overview_data(
        data = appdata, 
        expected_general_columns = metadata$general$item_name
      )
      expect_true(is.data.frame(test_results))
      expect_equal(
        names(test_results), 
        c("subject_id", "status", "WHO.classification", 
          "Age", "Sex", "event_name")
      )
    })
    
    it("creates the expected output", {
      expect_snapshot(
        get_static_overview_data(
          data = appdata, 
          expected_general_columns = metadata$general$item_name
          )
      )
    })
  }
)
