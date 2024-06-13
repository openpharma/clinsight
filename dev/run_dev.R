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
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  
  run_app(
    data_folder = temp_folder,
    test_mode = TRUE, 
    onStart = \(){onStop(\(){unlink(temp_folder, recursive = TRUE)})}
  )
} 

withr::with_envvar(list("GOLEM_CONFIG_ACTIVE" = "dev"), load_and_run_app())
