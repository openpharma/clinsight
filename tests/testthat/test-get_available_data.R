describe(
  paste0("get_available_data() creates a data frame with all available ", 
  "data per individual. It summarizes the available data points for each ", 
  "individual for each time point. For forms with a 'Name' column (mostly ",
  "common_forms but can also be study data forms) the Name column of the pivot ", 
  "table data will be used (for example, the specific adverse event or ", 
  "concomitant medication). For all other forms, the data points ", 
  "will be taken from event_name."), 
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
    it("Creates a data frame with the correct columns per individual. ", {
      testdata <- get_available_data(data = appdata, tables = apptables, 
                                     all_forms = all_forms)
      expect_true(is.data.frame(testdata))
      expect_equal(names(testdata), c("subject_id", "item_name", "form_repeat", 
                                      "item_group", "event_name", "event_label"))
    })
    it("Creates the expected data frame with given random appdata input", {
      expect_snapshot(
        get_available_data(data = appdata, tables = apptables, all_forms = all_forms)
      )
    })
    it("Adds a form_repeat number to item_name if duplicates occur within an 
       individual, to ensure item names can be uniquely identified", {
      df <- get_available_data(
        data = list(),
        tables = apptables["Adverse events"],
        all_forms = all_forms
        )
      # ID BEL_08_885 has two adverse events named 'Seizure'; these should show 
      # up with the correct form_repeat number in the item_name
      expect_equal(
        with(df, item_name[subject_id == "BEL_08_885" & grepl("Seizure", item_name)]),
        c("Seizure (N: 1)", "Seizure (N: 3)")
      )
    })
    it("can change the name of the form_repeat number that is written to the item_name 
       if duplicates occur", {
         df <- get_available_data(
           data = list(),
           tables = apptables["Adverse events"],
           all_forms = all_forms,
           form_repeat_name = "custom_name"
         )
         expect_equal(
           with(df, item_name[subject_id == "BEL_08_885" & grepl("Seizure", item_name)]),
           c("Seizure (custom_name: 1)", "Seizure (custom_name: 3)")
         )
       })
    it("Scenario 3 - Given ... and some forms defined in the metadata but 
    completely missing in the data or tables,
       I expect that I still get a table with available data", {
         #TODO: implement test
       })
  }
)
