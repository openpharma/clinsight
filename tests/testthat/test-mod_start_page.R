describe(
  "mod_start_page. Feature 1 | As a user, I want to be able to get an overview of all 
  the participants that I need to review on the start page. On the start page, 
  I need to be able to navigate to a selected participant or, if needed, to a 
  specific form of that selected participant.", 
  {
    testargs <- list(
      r = reactiveValues(), 
      rev_data = reactiveValues(), 
      navinfo = reactiveValues(),
      all_forms = data.frame(
        "main_tab" = c("Common events", "Study data", "Study data"), 
        "form" = c("Adverse events", "Vital signs", "ECG")
      )
    ) 
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_start_page_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_start_page_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_start_page_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
    # TODO: add test to navigate to participant (slightly duplicated with testing just the navigation module?)
    # TODO: add test to navigate to a form of a selected participant (slightly duplicated with testing just the navigation module?)
    
    it(
      "Scenario 1. Display overview of participants. 
        Given [preconditions], 
        and [inputs], 
        I expect that an overview of patients is displayed in the start page.", 
      {
        #TODO: implement test
        # browser()
        # 
        # static_overview_data <- get_static_overview_data(
        #   data = app_data,
        #   expected_general_columns = meta$general$item_name
        # )
        # 
        # testargs <- list(
        #   r = reactiveValues(), 
        #   rev_data = reactiveValues(
        #     overview =  
        #     )
        #   ), 
        #   navinfo = reactiveValues(),
        #   all_forms = data.frame(
        #     "main_tab" = c("Common events", "Study data", "Study data"), 
        #     "form" = c("Adverse events", "Vital signs", "ECG")
        #   )
        # )  
      # testServer(mod_start_page_server, args = testargs, {
      #   ns <- session$ns
      #   # example tests:
      #   # - Testing the setting of inputs
      #   # session()
      #   # session(x = 1)
      #   # expect_true(input == 1)
      #   # expect_true(r == 1)
      #   # expect_true(inherits(output, "html"))
      # })

    })
  }
)
 

