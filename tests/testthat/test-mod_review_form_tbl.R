describe(
  "mod_review_form_tbl. Feature 1 | Load application module in isolation.", 
  {
    testargs <- list(
      form = "Adverse events",
      form_data = reactiveVal(),
      form_review_data = reactiveVal(),
      form_items = "",
      active_subject = reactiveVal("DEU_02_482"),
      show_all = reactiveVal(TRUE),
      table_names = NULL,
      title = NULL
    ) 
    
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_review_form_tbl_ui(id = "test")
      golem::expect_shinytaglist(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_review_form_tbl_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    it("Can load the module server, with functioning internal parameters.", {
      testServer(mod_review_form_tbl_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)

describe(
  "mod_review_form_tbl. Feature 2 | View form tables. As a user, I want to 
  be able to view a table belonging to a form, in the correct format and with 
  selected rows passed through internal objects as as expected.", 
  {
    it(
      "Scenario 1 - Mark rows as selected for review. Given a test [Adverse events] data set,
        and the active subject_id set to ID 'DEU_02_482',
        and the subject having an adverse event [Allergic Reaction] that has not yet been reviewed,
        and setting the rows of the event [Allergic Reaction] to be selected in the form table,
        I expect that [pending_review_records] gets updated with the approriate row numbers,
        and that the output table is a valid JSON object.",
      {
        app_data <- get_appdata(clinsightful_data)
        ae_data <- app_data[["Adverse events"]]
        ae_rev_data <- get_review_data(ae_data) |> 
          dplyr::mutate(id = dplyr::row_number(), reviewed = "No", status = "new")
        testargs <- list(
          form = "Adverse events",
          form_data = reactiveVal(ae_data),
          form_review_data = reactiveVal(ae_rev_data),
          form_items = "",
          active_subject = reactiveVal("DEU_02_482"),
          show_all = reactiveVal(FALSE),
          table_names = NULL,
          title = NULL
        ) 
        
        testServer(mod_review_form_tbl_server, args = testargs, {
          ns <- session$ns
          
          session$userData$pending_form_review_status <- reactiveValues()
          session$userData$pending_review_records <- reactiveValues()
          session$userData$review_type <- reactiveVal("subject")
          session$flushReact()
          ae_rev_status <- merged_form_data() |> 
            dplyr::filter(
              subject_id == active_subject(), 
              grepl("Allergic Reaction", Name)
            ) |> 
            dplyr::pull(row_review_status)
          ae_rev_status <- ae_rev_status[[1]]
          ae_rev_status$review <- TRUE
          session$setInputs(
            table_review_selection = review_info_handler(ae_rev_status)
          )
          expect_equal(
            session$userData$pending_review_records[[form]],
            data.frame(id = ae_rev_status$ids, reviewed = "Yes")
          )
          expect_true(inherits(output[["table"]], "json"))
        }
        )
      }
    )
    it(
      "Scenario 2 - View table and mark row as review pending. 
          Given a test [Adverse events] data set,
          and the active subject_id set to ID 'DEU_02_482',
          I expect that the active data set [data_active] is a data frame object,
          and that this data frame contains only data of subject DEU_02_482,
          and that the output table is a valid JSON object with the expected number of rows,
          and that, after clicking the check box belonging to the first row,
          the item ids related to this row are marked as having a review pending.", 
      {
        app_data <- get_appdata(clinsightful_data)
        ae_data <- app_data[["Adverse events"]]
        ae_rev_data <- get_review_data(ae_data) |> 
          dplyr::mutate(id = dplyr::row_number(), reviewed = "No", status = "new")
        form_table <- get_form_table(
          ae_data, 
          ae_rev_data, 
          form = "Adverse events", 
          form_items = unique(ae_data$item_name), 
          active_subject = "DEU_02_482"
        )
        n_expected_rows <- form_table |> 
          dplyr::filter(subject_id == "DEU_02_482") |> 
          nrow()
        expected_reviewed_ids <- form_table[1,][["row_review_status"]][[1]]$ids
        
        test_ui <- function(request){
          tagList(
            shinyjs::useShinyjs(),
            golem_add_external_resources(),
            bslib::page(title = "Test Table", mod_review_form_tbl_ui("test"))
          )
        }
        test_server <- function(input, output, session){
          session$userData$pending_review_records <- reactiveValues()
          session$userData$pending_form_review_status <- reactiveValues()
          session$userData$review_type <- reactiveVal()
          
          mod_review_form_tbl_server(
            id = "test", 
            form = "Adverse events",
            form_data = reactiveVal(ae_data),
            form_review_data = reactiveVal(ae_rev_data),
            form_items = "",
            active_subject = reactiveVal("DEU_02_482"),
            show_all = reactiveVal(FALSE),
            table_names = NULL,
            title = "Test table"
          )
        }
        test_app <- shinyApp(test_ui, test_server, options = list("test.mode" = TRUE))
        app <- shinytest2::AppDriver$new(
          app_dir = test_app,
          name = "test-review_form_tbl",
          timeout = 8000,
          width = 1619,
          height = 955
        )
        withr::defer(app$stop())
        table_output <- app$get_value(output = "test-table")
        expect_true(inherits(table_output, "json"))
        expect_equal(
          length(app$get_value(input = "test-table_rows_all")),
          n_expected_rows
        )
        app$wait_for_js('$("#test-table input[type=\'checkbox\']").slice(0, 1).click()')
        rev_records <- app$get_values(export = "test-pending_review_records")$export
        
        expect_equal(
          rev_records$`test-pending_review_records`$id,
          expected_reviewed_ids
        )
      }
    )
  }
)


describe(
  "mod_review_form_tbl. Feature 3 | Download table. As a user, I want to 
  be able to download a table belonging to a form.", 
  {
    it(
      "Scenario 1 - Download a table. 
      Given a test [Medications] data set,
        and the active subject_id set to ID 'DEU_02_482',
        and ['show_all'] is set to FALSE,
        I expect that I can download the table with data of the selected subject,
        with the filename being 'clinsight.medication.DEU_02_482.csv',
        and that, after [show_all] is set to TRUE,
        I can download the data of all subjects of the respective form,
        with the file name being 'clinsight.medication.all_patients.csv'",
      {
        app_data <- get_appdata(clinsightful_data)
        med_data <- app_data[["Medication"]]
        med_rev_data <- get_review_data(med_data) |> 
          dplyr::mutate(id = dplyr::row_number(), reviewed = "No", status = "new")
        
        testargs <- list(
          form = "Medication",
          form_data = reactiveVal(med_data),
          form_review_data = reactiveVal(med_rev_data),
          form_items = "",
          active_subject = reactiveVal("DEU_02_482"),
          show_all = reactiveVal(FALSE),
          table_names = NULL,
          title = NULL
        ) 
        
        testServer(mod_review_form_tbl_server, args = testargs, {
          ns <- session$ns
          
          session$userData$pending_form_review_status <- reactiveValues()
          session$userData$pending_review_records <- reactiveValues()
          session$userData$review_type <- reactiveVal("subject")
          session$flushReact()
          
          download_link_name <- output$table_download
          expect_equal(
            basename(download_link_name),
            "clinsight.medication.DEU_02_482.csv"
          )
          table_one_subject <- readr::read_csv(
            download_link_name, 
            show_col_types = FALSE
          )
          expected_table_all <- merged_form_data() |> 
            dplyr::select(-row_review_status) |> 
            dplyr::mutate(dplyr::across(
              dplyr::where(is.character),
              \(x) gsub("<b>|</b>", "", x)
            )) 
          expected_table_one_subject <- expected_table_all |> 
            subset(subject_id == active_subject())
          expect_equal(table_one_subject, expected_table_one_subject)
          
          show_all(TRUE)
          session$flushReact()
          
          download_link_name <- output$table_download
          expect_equal(
            basename(download_link_name),
            "clinsight.medication.all_patients.csv"
          )
          
          table_all_subjects <- readr::read_csv(
            output$table_download, 
            show_col_types = FALSE
          )
          expect_equal(table_all_subjects, expected_table_all)
        }
        )
      }
    )
  }
)



