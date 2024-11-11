# Thu Jan 25 16:13:45 2024 ------------------------------
# LSA Useful for use as a minimal query review dataset, for testing modules that write 
# queries (for example, mod_query_follow_up). 

make_review_testdata <- function(){
  data.frame(
    subject_id = c("885", "361"),
    event_name = c("Any visit", "Visit 5"),
    item_group = c("Adverse events", "Vital signs"),
    form_repeat = c(1, 4),
    item_name = c("AE Number", "Systolic blood pressure"),
    event_date = c("2023-08-15", "2023-07-01"),
    edit_date_time = c("2023-09-30 01:01:01", "2023-08-30 01:01:01")
  ) |> 
    dplyr::mutate(
      reviewed = "No", 
      comment = "", 
      reviewer = "", 
      timestamp = "2000-01-01 01:01:01",
      status = "new"
    )
}

fixture_path <- system.file("tests/testthat/fixtures", package = "clinsight")
db_temp_connect(paste0(fixture_path, "/review_testdb.sqlite"), {
  if ("all_review_data" %in% DBI::dbListTables(con)) 
    DBI::dbRemoveTable(con, "all_review_data")
  db_add_primary_key(con, "all_review_data", make_review_testdata())
  db_add_log(con)
})
