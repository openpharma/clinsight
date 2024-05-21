pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

load_and_run_app <- function(){
  # withr call here does not work since the tempfile gets deleted during app initialization. 
  # different option below, using onStop() within the run_app() function to clean up.
  temp_folder <- tempdir()
  # Mimic data loading in production version: 
  data_path <- file.path(temp_folder, "clinsightful_data.rds")
  saveRDS(clinsightful_data, data_path)
  
  db_path <- file.path(temp_folder, "testdb.sqlite")
  file.copy("../testdb.sqlite", db_path)
  
  credentials_path <- file.path(temp_folder, "credentials.sqlite")
  file.copy("credentials.sqlite", credentials_path)

  run_app(
    meta = metadata, 
    data = data_path,
    user_db = db_path, 
    credentials_db = credentials_path, 
    credentials_pwd = "1234",
    test_mode = FALSE, 
    onStart = \(){onStop(\(){unlink(temp_folder, recursive = TRUE)})}
  )
} 
load_and_run_app()
