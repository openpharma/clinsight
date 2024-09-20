library(shinytest2)
describe(
  "mod_navigate_review. Feature 1 | As a user, I want to be able to see detailed 
  information about which forms need to be reviewed, and which queries are already queried. 
  This can be shown in tables. I want to be able to switch view so that I can 
  quickly filter only the forms of the active participant, or show data of all 
  participants of the selected sites.
  ", 
  {
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_navigate_review_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_navigate_review_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })

    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        r = reactiveValues(),
        rev_data = reactiveValues(summary = reactiveVal()),
        navinfo = reactiveValues(),
        all_forms = data.frame()
      )
      testServer(mod_navigate_review_server, args = testargs, {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
    it(
      "Scenario 1. Given Subject id set to 'subject01-test',
          and [input$show_all_data] set to TRUE/FALSE,
          I expect that the table header text will be 'All data'/'subject01-test'
          , respectively (in bold font)",
      {
        testargs <- list(
          r = reactiveValues(subject_id = "subject01-test"),
          rev_data = reactiveValues(summary = reactiveVal()),
          navinfo = reactiveValues(),
          all_forms = data.frame()
        )
        testServer(mod_navigate_review_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = TRUE)
          expect_equal(output[["header_text"]], "<b>All data</b>")
          session$setInputs(show_all_data = FALSE)
          expect_equal(output[["header_text"]], "<b>subject01-test</b>")
        })
      }
    )
    it(
      "Scenario 2. Given Subject id set to 'subject01-test',
          and [input$show_all_data] set to either TRUE/FALSE,
          and rev_data$summary (=reactiveVal()) containing summary data with at least the column subject_id,
          I expect that the table with review data shows either the summary
          review data from all subjects or only from the selected subject,
          and that the table with query data shows either summary review data from
          all subjects or only from the selected subject.",
      {
        summary_df <- data.frame(
          subject_id = c("subject01-test", paste0("subject", 2:4)),
          summary_col = paste0("summary_info", 1:4)
        )
        queries_df <- summary_df[, 1, drop = FALSE] |>
          dplyr::mutate(
            query = paste0("random query", 1:4),
            type = "Major",
            timestamp = "2023-11-01 01:01:01",
            query_id = paste0("query_id-", subject_id),
            resolved = "No",
            item = c("Intoxication", "Sepsis", "Pneumothorax", "Delirium"),
            item_group = "Adverse events",
            event_label = paste0("Visit ", 1:4)
          )

        testargs <- list(
          r = reactiveValues(
            subject_id = "subject01-test",
            query_data = queries_df
          ),
          rev_data = reactiveValues(summary = reactiveVal({summary_df})),
          navinfo = reactiveValues(),
          all_forms = data.frame()
        )
        query_table_data <- queries_df |> 
          dplyr::mutate(
            ID = paste0(item, " (", item_group, ", ", event_label, ")"),
            ID = ifelse(type == "Major", paste0(ID, " Major query"), ID)
          ) |> 
          dplyr::select(tidyr::all_of(c("subject_id", "ID", "query"))) 
        
        testServer(mod_navigate_review_server, args = testargs, {
          ns <- session$ns
          session$setInputs(show_all_data = TRUE)

          expect_equal(modal_rev_data(), summary_df)
          expect_true(inherits(output[["review_df"]], "json"))
          expect_equal(queries_table_data(), query_table_data)
          expect_true(inherits(output[["queries_table"]], "json"))

          session$setInputs(show_all_data = FALSE)
          expect_equal(modal_rev_data(), summary_df[1,])
          expect_true(inherits(output[["review_df"]], "json"))
          expect_equal(queries_table_data(), query_table_data[1, -1])
          expect_true(inherits(output[["queries_table"]], "json"))
        })
      }
    )
    it(
      "Scenario 3. Given Subject id set to 'subject01-test',
          and [input$show_all_data] set to either TRUE/FALSE,
          and rev_data$summary() (=reactiveVal()) containing an empty data frame,
          and r$query_data being an empty data frame, 
          I expect that an empty summary table and an empty summary will be shown",
      {
        #TODO: verify the JSON table outcomes?
        summary_df <- data.frame(
          subject_id = "",
          summary_col = ""
        )
        
        queries_df <- summary_df[, 1, drop = FALSE] |> 
          add_missing_columns(names(query_data_skeleton))
        
        testargs <- list(
          r = reactiveValues(
            subject_id = "subject01-test",
            query_data = queries_df[0,]
          ),
          rev_data = reactiveValues(summary = reactiveVal({summary_df[0,]})),
          navinfo = reactiveValues(),
          all_forms = data.frame()
        )
        testServer(mod_navigate_review_server, args = testargs, {
          ns <- session$ns
          
          session$setInputs(show_all_data = TRUE)
          expect_equal(modal_rev_data(), summary_df[0,])
          expect_true(inherits(output[["review_df"]], "json"))
          expect_true(inherits(output[["queries_table"]], "json"))

          session$setInputs(show_all_data = FALSE)
          expect_equal(modal_rev_data(), summary_df[0,])
          expect_true(inherits(output[["review_df"]], "json"))
        })
      }
    )
    it(
      "Scenario 4. Given Subject id set to 'subject01-test',
          and the input value [show_all_data] is set to either TRUE/FALSE,
          and the [summary()] data frame in [rev_data] contains summary review data,
          and I click on an external button to open the review data modal, 
          I expect that I can see the summary table with data of patient 'subject01-test',
          and that I can switch to data of all patients [show_all_data] to TRUE, 
          and that I can switch to the tab with the query table",
      {
        summary_df <- data.frame(
          subject_id = c("subject01-test", paste0("subject", 2:4)), 
          Form = "Adverse events", 
          summary_col = paste0("summary_info", 1:4)
        )
        
        queries_df <- summary_df[, 1, drop = FALSE] |> 
          dplyr::mutate(
            query = paste0("random query", 1:4),
            type = "Major",
            timestamp = "2023-11-01 01:01:01",
            query_id = paste0("query_id-", subject_id),
            resolved = "No",
            item = c("Intoxication", "Sepsis", "Pneumothorax", "Delirium"),
            item_group = "Adverse events",
            event_label = paste0("Visit ", 1:4)
          )
        
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            bslib::page_navbar(
              title = "Test review navigation", 
              header = bslib::layout_columns(
                mod_navigate_review_ui("test"),
                actionButton(inputId =  "open_modal_button", label = "Open modal")
              )
            )
          )
        }
        test_server <- function(input, output, session){
          rev_data <- reactiveValues(summary = reactiveVal({summary_df}), open_modal = FALSE, 
                                     forms_to_review = reactive({}), subject = "subject01-test")
          # LSA 13Dec23 
          # Creating a button to change rev_data$open_modal. Other option is to 
          # listen to the shinyjs::onclick() event, but I don't know how to 
          # activate this JS click programmatically. 
          mod_navigate_review_server(
            id = "test", 
            r = reactiveValues(
              subject_id = "subject01-test",
              query_data = queries_df
            ), 
            rev_data = rev_data,  
            navinfo = reactiveValues(active_form = "Adverse events", active_tab = "Common events"),
            all_forms = data.frame(),
            table_names = c("Subject" = "subject_id", "Form" = "item_group")
          )
          observeEvent(input$open_modal_button, {
            rev_data$open_modal <- FALSE
            rev_data$open_modal <- TRUE
          })
        }
        
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "test-mod_navigate_review",
          width = 1619, 
          height = 955, 
          timeout = 10000
          )
        withr::defer(app$stop())
        app$click("open_modal_button")
        app$wait_for_idle()
        app$expect_values(output = TRUE)
        app$set_inputs(`test-show_all_data` = TRUE)
        app$expect_values(output = TRUE)
        app$set_inputs(`test-review_tabs` = "Open queries")
        app$expect_values(output = TRUE)
      }
    )
    
  }
)


