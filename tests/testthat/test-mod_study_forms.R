describe(
  "mod_study_forms. Feature 1 | Load application module in isolation.",
  {
    it("Can load the module UI, with functioning internal parameters.", {
      ui <- mod_study_forms_ui(
        id = "test",
        form = "Vital signs",
        form_items = ""
      )
      golem::expect_shinytag(ui)
      # Check that formals have not been removed
      fmls <- formals(mod_study_forms_ui)
      for (i in c("id")){
        expect_true(i %in% names(fmls))
      }
    })
    
    it("Can load the module server, with functioning internal parameters.", {
      testargs <- list(
        form = "Vital signs",
        form_data = reactiveVal(),
        form_review_data = reactiveVal(),
        form_items = "",
        active_subject = reactiveVal("DEU_02_482"),
        id_item = c("subject_id", "event_name", "item_group", 
                    "form_repeat", "item_name"),
        table_names = NULL,
        item_info = data.frame()
      )
      testServer(mod_study_forms_server, args = testargs , {
        ns <- session$ns
        expect_true(inherits(ns, "function"))
        expect_true(grepl(id, ns("")))
        expect_true(grepl("test", ns("test")))
      })
    })
  }
)
describe(
  "mod_study_forms. Feature 2 | View forms with study-specific data. 
      As a user, I want to be able to view a form with
      study-specific data. The forms will be specified with a study-specific
      metadata file. ",
  {
    # If all data are specified as continuous data points, the module has two viewing modes:
    #   one showing interactive figures, with the data shown in continuous figures,
    # and one with the data in table form, with the data shown with their
    # original units (if applicable).
    set.seed(2023)
    appdata <- get_appdata(clinsightful_data)
    rev_data <- get_review_data(appdata[["Vital signs"]]) |>
      dplyr::mutate(
        id = dplyr::row_number(),
        reviewed = sample(c("Yes", "No"), dplyr::n(), replace = TRUE),
        status = sample(c("new", "old", "updated"), dplyr::n(), replace = TRUE)
      )
    form_items <- with(metadata$study_forms, item_name[item_group == "Vital signs"])
    form_items <- setNames(simplify_string(form_items), form_items)
    testargs <- list(
      form = "Vital signs",
      form_data = reactiveVal(appdata[["Vital signs"]]),
      form_review_data = reactiveVal(rev_data),
      active_subject = reactiveVal("NLD_05_561"),
      id_item = c("subject_id", "event_name", "item_group",
                  "form_repeat", "item_name"),
      form_items = form_items,
      table_names = NULL,
      item_info = metadata$form_level_data[metadata$form_level_data$item_group == "Vital signs", ]
    )
    it("Scenario 1 - Figure data. Given subject id is set to NLD_05_561, and form filter set to 'pulse' and
       'bmi', I expect that [fig_data] contains a data frame with only items 'BMI' and 'Pulse',
        and that a plotly [dynamic_figure] contains a plotly htmlwidget figure,
        and that the figure outputcontains a valid JSON object", {
          testServer(mod_study_forms_server, args = testargs, {
            ns <- session$ns
            session$setInputs(filter = c("pulse", "bmi"))
            expect_true(is.data.frame(fig_data()))
            expect_equal(as.character(unique(fig_data()$item_name)), c("BMI", "Pulse"))
            expect_equal(class(dynamic_figure()), c("plotly", "htmlwidget"))
            expect_true(inherits(output[["figure"]], "json"))
          })
        })

    it(
      "Scenario 3 - Table data without 'show_all'. Given subject id NLD_05_561,
          and input value 'show_all' is set to 'FALSE',
          I expect that a valid JSON output table will be created",
      {
        testServer(mod_study_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(
            filter = c("pulse", "BMI"),
            show_all = FALSE
          )
          expect_true(inherits(output[["review_form_tbl-table"]], "json"))
        })
      })
    it(
      "Scenario 4 - Given subject id NLD_05_561,
          and the input value [show_all] is set to 'TRUE',
          I expect that a valid JSON output table will be created", {
            testServer(mod_study_forms_server, args = testargs, {
              ns <- session$ns
              session$setInputs(
                filter = c("pulse", "BMI"),
                show_all = TRUE
              )
              expect_true(inherits(output[["review_form_tbl-table"]], "json"))
            })
          })
  }
)

describe(
  "Feature 4 | Highlight data that is not yet reviewed. 
    As a user, I want to be able to see data that has not yet been 
    reviewed highlighted", 
  {
    set.seed(2023)
    appdata <- get_appdata(clinsightful_data)
    rev_data <- get_review_data(appdata[["Vital signs"]]) |> 
      dplyr::mutate(
        id = dplyr::row_number(),
        reviewed = sample(c("Yes", "No"), dplyr::n(), replace = TRUE),
        status = sample(c("new", "old", "updated"), dplyr::n(), replace = TRUE)
      )
    
    testargs <- list(
      form = "Vital signs",
      form_data = reactiveVal(appdata[["Vital signs"]]),
      form_review_data = reactiveVal(rev_data),
      active_subject = reactiveVal("NLD_05_561"),
      id_item = c("subject_id", "event_name", "item_group", 
                  "form_repeat", "item_name"),
      form_items = with(metadata$study_forms, item_name[item_group == "Vital signs"]),
      item_info = metadata$form_level_data[metadata$form_level_data$item_group == "Vital signs", ]
    )
    it(
      "Scenario 1 - Review status information. Given subject id is set to '885',
        and the form set to 'Vital signs',
        and the filter set to 'pulse' and 'bmi',
        I expect that [fig_data] contains a column named 'reviewed',
        and that the 'reviewed' column contains the values 'Yes' or 'No'.", 
      {
        testServer(mod_study_forms_server, args = testargs, {
          ns <- session$ns
          session$setInputs(filter = c("pulse", "BMI"))
          expect_true(is.data.frame(fig_data()))
          expect_true("reviewed" %in% names(fig_data()))
          rev_col <- fig_data()$reviewed 
          expect_equal(sort(unique(rev_col)), c("No", "Yes"))
        })
      }
    )
    
    it(
      "Scenario 2 - Showing review status. 
        Given a test data set with random test data, 
        and [subject_id] is set to  'NLD_06_755',
        and form is set to 'Vital signs',
        and the filter is set to 'temperature',
        I expect that in the [fig_data] only temperature data is shown,
        with the first two measurements being old and the last being new,
        and that in the figure the data of subject 'NLD_06_755' is highlighted,
        with the last data point being shown as a bigger dot indicating it 
        needs to be reviewed,
        and that, after switching to table view,
        it shows the table with not yet reviewed data highlighted.",
      {
        form_items <- with(metadata$study_forms, item_name[item_group == "Vital signs"])
        form_items <- setNames(simplify_string(form_items), form_items)
        test_ui <- function(request){
          tagList(
            golem_add_external_resources(),
            shinyjs::useShinyjs(),
            bslib::page_navbar(
              mod_study_forms_ui(
                "test", 
                form = "Vital signs", 
                form_items = form_items
              )
            ),
          )
        }
        
        test_server <- function(input, output, session){
          mod_study_forms_server(
            id = "test",
            form = "Vital signs",
            form_data = reactiveVal(appdata[["Vital signs"]]),
            form_review_data = reactiveVal(rev_data),
            active_subject = reactiveVal("NLD_06_755"),
            id_item = c("subject_id", "event_name", "item_group", 
                        "form_repeat", "item_name"),
            form_items = form_items,
            item_info = metadata$form_level_data[metadata$form_level_data$item_group == "Vital signs", ]
          )
        }
        test_app <- shinyApp(test_ui, test_server)
        app <- shinytest2::AppDriver$new(
          app_dir = test_app, 
          name = "study_forms",
          width = 1619, 
          height = 955
        )
        withr::defer(app$stop())
        app$set_inputs("test-filter" = "temperature")
        app$wait_for_idle(1100)
        app$expect_values(output = TRUE, export = TRUE)
        df <- app$get_value(export = "test-fig_data")
        expect_equal(as.character(unique(df$item_name)), "Temperature")
        expect_equal(with(df, reviewed[subject_id == "NLD_06_755"]), c("Yes", "Yes", "No"))
        app$set_inputs("test-switch_view" = "table")
        app$wait_for_idle()
        app$expect_values(output = TRUE, export = TRUE)
      }
    )
    
  }
)

