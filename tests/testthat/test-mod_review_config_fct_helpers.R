describe(
  "filter_data() works", 
  {
    app_data <- list(
      "ECG" = data.frame(
        subject_id = c("Subj01", "Subj02", "Subj03"),
        site_code = c("NL01", "DE03", "AU01")
      ),
      "not_included" = data.frame(
        subject_id = c("Subjx"),
        site_code = c("Sitex")
      )
      )
    app_tables <- list(
      "ECG" = data.frame(
        subject_id = c("Subj01", "Subj02", "Subj03"),
        site_code = c("NL01", "DE03", "AU01")
      ),
      "not_included" = data.frame(
        subject_id = c("Subjx"),
        site_code = c("Sitex")
      )
    )
    
    it("Filters lists of appdata and apptables with the required sites, and 
       returns the data in a reactiveValues object.", {
      rvals <- reactiveValues()
      outcome <- reactive({
        filter_data(
        rvals, 
        sites = c("NL01", "DE03"), 
        subject_ids = c("Subj01", "Subj02", "Subj03"), 
        appdata = app_data, 
        apptables = app_tables
        )
      })
      expect_true(is.reactive(outcome))
      outcome.list <- isolate(reactiveValuesToList(outcome()))
      expect_equal(
        names(outcome.list), 
        c("filtered_data", "filtered_subjects", "filtered_tables", "subject_id")
      )
      expect_equal(
        outcome.list$filtered_subjects, 
        c("Subj01", "Subj02")
        )
      expect_equal(
        outcome.list$filtered_data$ECG$site_code,
        c("NL01", "DE03")
      )
      expect_equal(
        outcome.list$filtered_data,
        outcome.list$filtered_tables
      )
      
    })
  }
)
