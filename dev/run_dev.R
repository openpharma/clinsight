# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload(export_all = TRUE)

# Run the application
load_and_run_app <- function(){
  # withr call here does not work since the tempfile gets deleted during app initialization. 
  # different option below, using onStop() within the run_app() function to clean up.
  temp_folder <- tempdir()
  db_path <- file.path(temp_folder, "testdb.sqlite")
  
  run_app(
    meta = metadata, 
    data = clinsightful_data,
    user_db = db_path, 
    test_mode = TRUE, 
    onStart = \(){onStop(\(){unlink(temp_folder, recursive = TRUE)})}
  )
} 

load_and_run_app()