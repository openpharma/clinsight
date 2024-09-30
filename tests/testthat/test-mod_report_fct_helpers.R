describe(
  "create_report() Feature 1 | Create PDF report. 
    As a user, I want to be able to create a PDF report showing 
    either, depending on the user settings, the activities of the reviewer during 
    a session, or an overview of the review status at a certain date, showing all 
    data of a patient. ", 
  {
    it(
      "Scenario 1 - Create report. Given a Report template at inst/app/Report.Rmd,
        and input of a reactiveValue r containing the user name and the review 
        data with all sites reviewed, 
        and a data frame [review_df] with the review information,
        and a data frame [query_df] with the queries raised,
        and fileinput set to the location at which the file should be saved,
        I expect that a PDF report will be created at the location of [fileinput]. ", 
      {
        query_df <- data.frame(
          "query_id"      = c("ID1-unique_id", "ID2-unique_id"),
          "ID"            = c("ID1", "ID2"),
          "Form"          = c("Vital signs", "Adverse events"),
          "Event"         = c("Visit 1"),
          "Item"          = c("Pulse", "Sepsis"),
          "Time"          = c("2023-01-01 01:01:01 UTC", "2023-11-02 01:01:01 UTC"),
          "n"             = c(1),     
          "Query"         = c("Query text test.", "Scoring correct? Please verify"),
          "Author"        = c("Test author", "Author3"),
          "resolved"      = c("No", "Yes"),
          "resolved_date" = c("", "2023-11-15 01:01:01"),
          "edit_reason"   = c("", "")
        )
        review_df <- data.frame(
          "ID" = "Test_name",
          "Event" = "Visit 1",
          "Form" = "Test_group",
          "Item" = "Test_item",
          "Edit date" = "2023-11-05 01:26:00",
          "reviewed" = "Yes",
          "comment" = "",
          "Reviewer" = "Admin",
          "Time" = "2023-11-13 01:01:01",
          "status" = "old"
        )
        expect_message(
          create_report(fileinput = "tempreport.pdf", reviewer = "Admin", study_sites = c("NL01", "DE01"),
                        review_df, query_df), 
          "Output created"
          )
      }
    )
  }
)

