describe("mod_navigate_forms. Feature 1 | As a user, I want to be able to start the 
  module in isolation", {
    navinfo <- reactiveValues(active_form = "Adverse events", active_tab = "Common events", 
                              trigger_page_change = 0)
    review_forms <- reactive({"Adverse events"})
    forms <-  data.frame(
      "main_tab" = c("Common events", "Study data", "Study data"), 
      "form" = c("Adverse events", "Vital signs", "ECG")
    )
    testargs <- list(navinfo = navinfo, forms_to_review = review_forms, 
                     all_forms = forms)
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_navigate_forms_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_navigate_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_navigate_forms_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
})

describe(
  "mod_navigate_forms. Feature 2 | As a user, I want to be able to navigate forms in the application, using 
  the back and forward buttons in the application. When clicking on the forward 
  button, I should navigate to the next form of the same patient.", {
    navinfo <- reactiveValues(active_form = "Adverse events", active_tab = "Common events", 
                              trigger_page_change = 0)
    review_forms <- reactive({"Adverse events"})
    forms <-  data.frame(
      "main_tab" = c("Common events", "Study data", "Study data"), 
      "form" = c("Adverse events", "Vital signs", "ECG")
    )
    testargs <- list(navinfo = navinfo, forms_to_review = review_forms, 
                     all_forms = forms)
    
    it("Scenario 1 - New data is displayed in bold font face. I expect that a 
        bold form name will be displayed if the form is in the vector review_forms 
       (which contains all form names of the forms with new/updated data)", {
         testServer(mod_navigate_forms_server, args = testargs, {
           ns <- session$ns 
           expect_equal(output[["form_name"]], "<center><b>Adverse events</b></center>")
         })
       })
    
    it("Scenario 2 - Already reviewed data is shown in normal font face. I expect 
       that the form_name is shown in a non-bold font face", {
         testServer(mod_navigate_forms_server, args = testargs, {
           ns <- session$ns 
           navinfo$active_form <- "Vital signs"
           session$flushReact()
           expect_equal(output[["form_name"]], "<center>Vital signs</center>")
         })
       })
    
    it("Scenario 3 - Navigating to next form. I expect that I can navigate to 
       the next form if requested", {
         testServer(mod_navigate_forms_server, args = testargs, {
           ns <- session$ns 
           navinfo$active_form <- "Adverse events"
           session$setInputs(form_next = 1)
           expect_equal(navinfo$active_tab, "Study data")
           expect_equal(navinfo$active_form, "Vital signs")
         })
       })
    
    it("Scenario 4 - Navigating to previous form. I expect that I can navigate 
       to the previous form if requested", {
         testServer(mod_navigate_forms_server, args = testargs, {
           ns <- session$ns 
           session$setInputs(form_previous = 1)
           expect_equal(navinfo$active_tab, "Common events")
           expect_equal(navinfo$active_form, "Adverse events")
         })
       })
    
    it("Scenario 5 - Navigation to previous form when first form is active. I 
        expect that I navigate to the last form if the first form is active and 
       the previous button is pressed", {
         testServer(mod_navigate_forms_server, args = testargs, {
           ns <- session$ns 
           session$setInputs(form_previous = 2)
           expect_equal(navinfo$active_tab, "Study data")
           expect_equal(navinfo$active_form, "ECG")
         })
       })
    
    it("Scenario 6 - Navigation to next form when last form is active. I 
        expect that I navigate to the first form if the last form is active and 
       the next button is pressed", {
         testServer(mod_navigate_forms_server, args = testargs, {
           ns <- session$ns 
           navinfo$active_form <- "ECG"
           session$setInputs(form_next = 4)
           expect_equal(navinfo$active_tab, "Common events")
           expect_equal(navinfo$active_form, "Adverse events")
         })
       })
    
    it("Scenario 7 - Uncommon change of tabs. I expect that the module does not 
        error if a change in tabs is requested and the main tab is named other 
        than 'Common events'  or 'Study data'. ", {
          testServer(mod_navigate_forms_server, args = testargs, {
            ns <- session$ns 
            navinfo$active_tab <- "Start"
            session$setInputs(form_next = 3)
            expect_equal(navinfo$active_tab, "Study data")
            expect_equal(navinfo$active_form, "Vital signs")
          })
        })
    
    it("Scenario 8 - Using non-existing forms or empty forms numbers. I expect 
        that there will be a warning when a non-existing form is used or an empty 
        form number is given, and that the page change is aborted, and that the 
       module does not crash", {
         testServer(mod_navigate_forms_server, args = testargs, {
           ns <- session$ns 
           navinfo$active_form <- "x"
           expect_warning(session$setInputs(form_previous = 3))
           suppressWarnings(session$setInputs(form_previous = 4))
           expect_equal(navinfo$active_tab, "Study data")
           expect_equal(navinfo$active_form, "x")
         })
       })
  })



