describe(
  "mod_go_to_form. Feature 1 | Load application module in isolation.", 
  {
    testargs <- list(
      r = reactiveValues(subject_id = "Subj1", filtered_subjects = "Subj1"),
      navinfo = reactiveValues(), 
      navtable = reactiveVal({data.frame()}),
      tablerow = reactiveVal(2),
      all_forms = data.frame(),
      form_name = "Form",
      subject_id = "subject_id"
    )
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_go_to_form_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_go_to_form_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_go_to_form_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)
describe(
  paste0("mod_go_to_form. Feature 2 | Navigate to patient selected in a table. ", 
         "As a user, I want to be able to navigate ", 
         "to the patient in the selected table row.", 
         "If the table row contains a form name, I want to navigate to that form of the", 
         "applicable patient directly"), 
  {
    testargs <- list(
      r = reactiveValues(subject_id = "Subj1", filtered_subjects = c("Subj1", "Subj2")),
      # navinfo contains active_form, active_tab, and trigger_page_change. 
      # no need to set these since the module will only write to these, and does 
      # not need to read them for proper functioning.
      navinfo = reactiveValues(), 
      navtable = reactiveVal({
        data.frame(
          "subject_id" = c("Subj1", "Subj2"),
          Form = c("Adverse events", "ECG")
        )
      }),
      tablerow = reactiveVal(2),
      all_forms = data.frame(
        "main_tab" = c("Common events", "Study data", "Study data"), 
        "form" = c("Adverse events", "Vital signs", "ECG")
      ),
      form_name = "Form",
      subject_id = "subject_id"
    )
    it("Scenario 1 - Browse to a dedicated patient and form.", {
      testServer(mod_go_to_form_server, args = testargs, {
        ns <- session$ns
        # twice because ignoreinit=TRUE in the module:
        session$setInputs(go_to_form = 1)
        session$setInputs(go_to_form = 2)
        expect_equal(navinfo$active_form, "ECG")
        expect_equal(navinfo$active_tab, "Study data")
        expect_equal(navinfo$trigger_page_change, 1)
        expect_equal(r$subject_id, "Subj2")
        
        tablerow(1)
        session$flushReact()
        session$setInputs(go_to_form = 5)
        expect_equal(navinfo$active_form, "Adverse events")
        expect_equal(navinfo$active_tab, "Common events")
        expect_equal(navinfo$trigger_page_change, 2)
        expect_equal(r$subject_id, "Subj1")
      })
    })
    it("Scenario 2 - Ignores form_name if form_name is null and browses to the first 
       form of the patient", {
         testargs["form_name"] <- list(NULL) 
         testServer(mod_go_to_form_server, args = testargs, {
           ns <- session$ns
           tablerow(2)
           session$setInputs(go_to_form = 1)
           session$setInputs(go_to_form = 2)
           expect_equal(navinfo$active_form, "Adverse events")
           expect_equal(navinfo$active_tab, "Common events")
           expect_equal(r$subject_id, "Subj2")
         })
       })
    it("Scenario 3 - Multiple rows selected. I expect that it warns that only 
          the first row will be used if multiple tablerows are selected", {
            testServer(mod_go_to_form_server, args = testargs, {
              ns <- session$ns
              tablerow(c(2, 3))
              expect_warning({
                session$setInputs(go_to_form = 1)
                session$setInputs(go_to_form = 2)
              })
            })
          })
    
    it("Scenario 4 - form_name missing. I expect that it warns and aborts if 
       form_name is not found in the table", {
      testargs <- list(
        r = reactiveValues(),
        navinfo = reactiveValues(), 
        navtable = reactiveVal(data.frame("subject_id" = "", "Form" = "")),
        tablerow = reactiveVal(2),
        all_forms = data.frame("main_tab" = "", "form" = ""),
        form_name = "Unknown_formname_column"
      )
      testServer(mod_go_to_form_server, args = testargs, {
        ns <- session$ns
        session$setInputs(go_to_form = 1)
        expect_warning(
          session$setInputs(go_to_form = 2), 
          "form_name 'Unknown_formname_column' not found in the table. Go to form aborted"
        )
      })
    })
    
    it("Scenario 5 - subject_id column missing. I expect that it warns and aborts 
       if subject_id column is not found in the table", {
      testargs <- list(
        r = reactiveValues(),
        navinfo = reactiveValues(), 
        navtable = reactiveVal(data.frame("subject_id" = "", "Form" = "")),
        tablerow = reactiveVal(2),
        all_forms = data.frame("main_tab" = "", "form" = ""),
        subject_id = "unknown_subjectname_column"
      )
      testServer(mod_go_to_form_server, args = testargs, {
        ns <- session$ns
        session$setInputs(go_to_form = 1)
        expect_warning(
          session$setInputs(go_to_form = 2),
          "subject_id 'unknown_subjectname_column' not found in the table. Go to form aborted"
        )
      })
    })
    
    
    it(
      "Scenario 6 - Empty subject id row. I expect that it warns and aborts if 
      the selected subject id row is NULL", 
      {
        test_args <- list(
          r = reactiveValues(),
          navinfo = reactiveValues(), 
          navtable = reactiveVal(data.frame("subject_id" = "", "Form" = "")),
          tablerow = reactiveVal(3),
          all_forms = data.frame()
        )
        testServer(mod_go_to_form_server, args = test_args, {
          ns <- session$ns
          session$setInputs(go_to_form = 1)
          expect_warning(
            session$setInputs(go_to_form = 2), 
            "Invalid subject_id selected in table. Go to form aborted"
          )
        })
      }
    )
    
    it(
      "Scenario 7 - Unknown subject_id. I expect that it warns and aborts if an 
      unknowwn subject_id was selected in the table", 
      {
        test_args <- list(
          r = reactiveValues(filtered_subjects = "Subj1"),
          navinfo = reactiveValues(), 
          navtable = reactiveVal({
            data.frame(
              "subject_id" = c("Subj1", "Subj_unknown"),
              "Form" = c("Adverse events", "ECG")
            )
          }),
          tablerow = reactiveVal(2),
          all_forms = data.frame()
        )
        
        testServer(mod_go_to_form_server, args = test_args, {
          ns <- session$ns
          session$setInputs(go_to_form = 1)
          expect_warning(
            session$setInputs(go_to_form = 4), 
            "subject_id in table not in list of known subject ids"
          )
          
          # if a snapshot test fails, it might give a strange error similar to this one:
          # https://github.com/r-lib/testthat/issues/1646#issue-1279803462
          # The probably means that the snapshot test is failing. 
          # Check if there is a snapshot 'mod_go_to_form_new.md'. If so, you can call 
          # testthat::snapshot_review("mod_go_to_form"). If not, 
          # debug manually using browser(). 
        })
      }
    )
    
    it(
      "Scenario 8 - Form name missing. Warns and aborts if the form name is missing in the selected table row", 
      {
        test_args <- list(
          r = reactiveValues(subject_id = "Subj1", filtered_subjects = "Subj1"),
          navinfo = reactiveValues(), 
          navtable = reactiveVal(data.frame("subject_id" = "Subj1", "Form" = "")),
          tablerow = reactiveVal(1),
          all_forms = data.frame("main_tab" = "Common events", "form" = "Adverse events")
        )
        testServer(mod_go_to_form_server, args = test_args, {
          ns <- session$ns
          session$setInputs(go_to_form = 1)
          expect_warning(
            session$setInputs(go_to_form = 2),
            "No valid active form selected. Go to form aborted"
          )
        })
      }
    )
    
  }
)

