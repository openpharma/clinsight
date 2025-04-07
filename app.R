# Launch the ShinyApp (Do not remove this comment)

# This file and the manifest.json are needed for Posit Connect deployments.
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
# update manifest file
# if absolute path is needed for renv profile
# Sys.setenv(RENV_PROFILE = "full")
Sys.setenv(RENV_PROFILE = "minimal")
# Install {clinsight}
# remotes::install_github(
#   "openpharma/clinsight",
#   ref = "dev",
#   force = TRUE
# )
# packageVersion("clinsight") 
# renv::snapshot()
# renv::paths$lockfile() # test
# assignInNamespace(
#   "renvLockFile",
#   \(...) renv::paths$lockfile(),
#   "rsconnect"
# )
# rsconnect::writeManifest() # run if needed
# clinsight::run_app() # add parameters here (if any)

# Run the application
load_and_run_app <- function(){
  app_data_folder <- "./app/"
  if(!dir.exists(app_data_folder)) dir.create(app_data_folder)
  old_golem_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = "dev")
  
  clinsight::run_app(
    data_folder = app_data_folder,
    onStart = \(){
      onStop(\(){
    #   unlink(temp_folder, recursive = TRUE); 
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_golem_config)
      }
    )}
  )
} 

load_and_run_app()
