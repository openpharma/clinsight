# will be used in test apps for end-to-end tests. Useful since the db synchronization time can be fixed in this database.
# Creating a database will decrease the time to run the application

data_folder <- testthat::test_path("fixtures")
rev_data <- get_review_data(clinsightful_data)
db_path <- file.path(data_folder, "testdb.sqlite")
db_create(rev_data, db_path)
# fixed synch time needed for snapshots:
DBI::dbWriteTable(get_db_connection(db_path), "db_synch_time",
                  data.frame("synch_time" = "2024-01-10"), overwrite = TRUE)

