describe("mod_navigate_participants. Feature 1 | As a user, I want to be able to start the 
  module in isolation", {
    testargs <- list(
      r = reactiveValues(
        filtered_tables = list(
          General = data.frame(
            subject_id = c("Subj1", "Subj2", "Subj3"),
            status_label = c("lab1", "lab2", "lab3")
          )
        ),
        subject_id = "",
        filtered_subjects = c("Subj1", "Subj2", "Subj3")
      )
    ) 
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_navigate_participants_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_navigate_participants_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_navigate_participants_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  })

describe(
  "mod_navigate_participants. Feature 2 | As a user, I want to be able to browse 
  through the study participants by means of a back and forward button, or with 
  a drop-down menu. I want to be able to see some summary information of the 
  selected patient. After pressing save, the patient should be selected as 
  active patient in the app.", 
  {
    testargs <- list(
      r = reactiveValues(
        filtered_tables = list(
          General = data.frame(
            subject_id = c("Subj1", "Subj2", "Subj3"),
            status_label = c("lab1", "lab2", "lab3")
          )
        ),
        subject_id = "",
        filtered_subjects = c("Subj1", "Subj2", "Subj3")
      )
    ) 
    it(
      "Scenario 1 - Given a table with general information in reactiveValues [r], 
        and the selected participant set to 'Subj2',
        and pressing the apply button (setting [subj_apply] to 2),
        I expect that the global active subject_id in reactivevalues [r] is set to 'Subj2'", 
      {
        testServer(mod_navigate_participants_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            participant_selection = "Subj2",
            subj_apply = 1
          )
          expect_equal(r$subject_id, "Subj2")
          expect_equal(output$status, "lab2")
          expect_true("html" %in% names(output$subject_info))
        })
      }
    )
    it(
      "Scenario 2 - Given a table with general information in reactiveValues [r], 
          and the selected participant set to 'Subj2',
          and pressing the next or previous button,
          I expect that the selected subject will be the nex or previous subject.", 
      {
        testServer(mod_navigate_participants_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            participant_selection = "Subj1",
            subj_next = 1
          )
          session$flushReact()
          expect_equal(active_subject(), "Subj2")
          session$setInputs(subj_previous = 1)
          expect_equal(active_subject(), "Subj1")
        })
      }
    )
    it(
      "Scenario 3 - Given a table with general information in reactiveValues [r], 
          and the selected participant set to the FIRST subject in the table, 
          and pressing the back button,
          I expect that the active subject ID does not change", 
      {
        testServer(mod_navigate_participants_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            participant_selection = "Subj1",
            subj_previous = 3
          )
          expect_equal(active_subject(), "Subj1")
        })
      }
    )
    it(
      "Scenario 4 - Given a table with general information in reactiveValues [r], 
          and the selected participant set to the LAST subject in the table, 
          and pressing the next button,
          I expect that the active subject ID does not change", 
      {
        testServer(mod_navigate_participants_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            participant_selection = "Subj3",
            subj_next = 3)
          expect_equal(active_subject(), "Subj3")
          # input$participant_selection does not change in this case since it uses 
          # an update_* function in the server to change the input, which cannot 
          # be tested with testServer().
        })
      }
    )
    it(
      "Scenario 5 - Given a table with general information in reactiveValues [r], 
          and the active global subject in [r] is set to a non-existent one,
          I expect that [pt_info] contains a message with the text 'Data missing'", 
      {
        testServer(mod_navigate_participants_server, args = testargs, {
          ns <- session$ns
          r$subject_id <- "unknown subject"
          session$flushReact()
          expect_equal(subject_info()$pt_info, HTML("<i>! Data missing</i>"))
        })
      }
    )
    it(
      "Scenario 6 - Given a table with general information in reactiveValues [r], 
      and the active [subject_id] set to 'Subj1',
      and the actions 
      and clicking the subject tile in the header, 
      I expect that a module will be shown with participant information, 
      and with options to change the current participant.", 
      {
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page_navbar(header = 
                                 bslib::card(mod_navigate_participants_ui("test"))
            )
          )
        }
        test_server <- function(input, output, session){
          mod_navigate_participants_server(
            id = "test", 
            r = reactiveValues(
              filtered_tables = list(
                General = data.frame(
                  subject_id = c("Subj1", "Subj2", "Subj3"),
                  status_label = c("lab1", "lab2", "lab3"), 
                  Sex = c("Male", "Female", "Female"),
                  Age = c(70, 16, 29)
                )
              ),
              subject_id = "Subj1",
              filtered_subjects = c("Subj1", "Subj2", "Subj3")
            )
          )
        }
        test_app <- shinyApp(test_ui, test_server, options = list("test.mode" = TRUE))
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "nav-subj",
          width = 1000, 
          height = 670
        )
        withr::defer(app$stop())
        
        app$expect_values()
        
        app$wait_for_js("$('#test-subject_info').click()")
        input.names <- names(app$get_values()$input)
        random.input.name <- input.names[grepl("test-shinyjs-test", input.names)]
        # Only record output, not input, since input contains a random string 
        # produced by shinyjs (js click event), and the 
        # string cannot be controlled using a fixed set.seed in appdriver  
        app$expect_values(output = TRUE)
        app$click("test-subj_next")
        app$expect_values(output = TRUE)
        
        app$click("test-subj_apply")
        app$expect_values(output = TRUE)
        
      }
    )
    
  }
)


