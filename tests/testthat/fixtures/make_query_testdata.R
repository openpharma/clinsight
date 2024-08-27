# Thu Jan 25 16:13:45 2024 ------------------------------
# LSA Useful for use as a minimal query database, for testing modules that write 
# queries (for example, mod_query_follow_up). 

make_query_testdata <- function(){
  data.frame(
    "query_id"      = c("ID1-unique_id", "ID2-unique_id"),
    "type"          = c("Normal"),
    "subject_id"    = c("ID1"),
    "event_label"   = c("Visit 1"),
    "item_group"    = c("Vital signs", "Adverse events"),
    "item"          = c("Pulse", "Sepsis"),
    "timestamp"     = c("2023-01-01 01:01:01 UTC", "2023-11-02 01:01:01 UTC"),
    "n"             = c(1),     
    "reviewer"      = c("Test author (Administrator)", "Author3 (Medical Monitor)"),
    "query"         = c("Query text test.", "Scoring correct? Please verify"),
    "resolved"      = c("No"),
    "resolved_date" = NA_character_,
    "edit_reason"   = NA_character_
  )
}

fixture_path <- system.file("tests/testthat/fixtures", package = "clinsight")
saveRDS(object = make_query_testdata(), 
        file = paste0(fixture_path, "/query_testdata.rds") 
)

