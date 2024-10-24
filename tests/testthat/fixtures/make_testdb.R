# will be used in test apps for end-to-end tests. Useful since the db synchronization time can be fixed in this database.
# Creating a database will decrease the time to run the application

data_folder <- testthat::test_path("fixtures")
rev_data <- get_review_data(clinsightful_data)
db_path <- system.file(data_folder, "testdb.sqlite", package = "clinsight")
db_create(rev_data, db_path)
# fixed synch time needed for snapshots:
db_temp_connect(db_path, {
  if ("db_synch_time" %in% DBI::dbListTables(con)) 
    DBI::dbRemoveTable(con, "db_synch_time")
  db_add_primary_key(
    con, 
    "db_synch_time",
    data.frame("synch_time" = "2023-09-15 10:10:00 UTC")
  )
})

# To inspect the table, you can run something like this:
# all_tables <- db_temp_connect(db_path, DBI::dbListTables(con))
# db_temp_connect(db_path, lapply(setNames(nm = all_tables), \(x){
#   dplyr::tbl(con, x) |> dplyr::collect()
#   }))
