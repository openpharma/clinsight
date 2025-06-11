
db_update_version <- function(db_path, version = "1.1") {
  stopifnot(file.exists(db_path))
  version <- match.arg(version)
  temp_path <- withr::local_tempfile(fileext = ".sqlite")
  file.copy(db_path, temp_path)
  con <- get_db_connection(temp_path)
  
  current_version <- tryCatch({
    DBI::dbGetQuery(con, "SELECT version FROM db_version") |> 
      unlist(use.names = FALSE)}, error = \(e){""})
  if(identical(current_version, db_version)) return("Database up to date. No update needed")
  
  review_skeleton <- DBI::dbGetQuery(con, "SELECT * FROM all_review_data LIMIT 0")
  rs <- DBI::dbSendQuery(con, "ALTER TABLE all_review_data RENAME TO all_review_data_old")
  DBI::dbClearResult(rs)
  rs <- DBI::dbSendQuery(con, "ALTER TABLE query_data RENAME TO query_data_old")
  DBI::dbClearResult(rs)
  
  new_pk_data <- list(
    "all_review_data" = review_skeleton,
    "query_data"      = query_data_skeleton
  )
  idx_pk_cols <- list(
    all_review_data = idx_cols
  )
  other_data <- list(
    "db_version" = data.frame(version = db_version)
  )
  db_add_tables(con, new_pk_data, idx_pk_cols, other_data)
  
  query_cols <- paste(names(query_data_skeleton), collapse = ", ")
  cat("\nInserting old query records into new table.\n")
  rs <- DBI::dbSendStatement(con, sprintf("INSERT INTO query_data (%1$s) SELECT %1$s FROM query_data_old", query_cols))
  DBI::dbClearResult(rs)
  
  stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM query_data") == 
              DBI::dbGetQuery(con, "SELECT COUNT(*) FROM query_data_old"))
  
  rs <- DBI::dbSendStatement(con, "DROP TABLE query_data_old")
  DBI::dbClearResult(rs)
  
  cat("\nInserting old review records into new tables.\n")
  cols_to_update <- names(review_skeleton)[!names(review_skeleton) %in% idx_pk_cols$all_review_data]
  cols_to_insert <- names(review_skeleton) |> 
    paste(collapse = ", ")
  upsert_statement <- paste(
    "INSERT INTO",
    "all_review_data",
    sprintf("(%s)", cols_to_insert),
    sprintf("SELECT %s FROM all_review_data_old WHERE true", cols_to_insert),
    "ON CONFLICT",
    sprintf("(%s)", paste(idx_pk_cols$all_review_data, collapse = ", ")),
    "DO UPDATE SET",
    sprintf("%1$s = excluded.%1$s", cols_to_update) |> paste(collapse = ", ")
  )
  rs <- DBI::dbSendStatement(con, upsert_statement)
  DBI::dbClearResult(rs)
  
  stopifnot(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM all_review_data") +
              DBI::dbGetQuery(con, "SELECT COUNT(*) FROM all_review_data_log") == 
              DBI::dbGetQuery(con, "SELECT COUNT(*) FROM all_review_data_old"))
  
  rs <- DBI::dbSendStatement(con, "DROP TABLE all_review_data_old")
  DBI::dbClearResult(rs)
  
  file.copy(temp_path, db_path, overwrite = TRUE)
  cat("Finished updating to new database standard\n\n")
}
