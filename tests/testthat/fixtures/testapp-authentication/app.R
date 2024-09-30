pkg_name <- "clinsight"
library(pkg_name, character.only = TRUE)

load_and_run_app <- function(){
  # withr call here does not work since the tempfile gets deleted during app initialization. 
  # different option below, using onStop() within the run_app() function to clean up.
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "shinymanager")
  # Mimic data loading in production version: 
  saveRDS(clinsightful_data, file.path(temp_folder, "study_data.rds"))
  saveRDS(metadata, file.path(temp_folder, "metadata.rds"))  
  file.copy("../testdb.sqlite", file.path(temp_folder, "user_db.sqlite"))
  file.copy("credentials.sqlite", file.path(temp_folder, "credentials_db.sqlite"))

  run_app(
    data_folder = temp_folder,
    credentials_pwd = "1234",
    onStart = \(){onStop(\(){
      unlink(temp_folder, recursive = TRUE); 
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_golem_config)
    })}
  )
} 

load_and_run_app()
