# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
# Using golem::pkg_path() is needed here, see:
# https://github.com/openpharma/clinsight/issues/69#issue-2504462187
golem::document_and_reload(pkg = golem::pkg_path(), export_all = TRUE)

# Run the application
load_and_run_app <- function(){
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "dev")
  
  run_app(
    data_folder = temp_folder,
    onStart = \(){onStop(\(){
      unlink(temp_folder, recursive = TRUE); 
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_golem_config)
    })}
  )
} 

load_and_run_app()
