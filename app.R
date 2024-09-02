# Launch the ShinyApp (Do not remove this comment)

# This file and the manifest.json are needed for Posit Connect deployments.
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
# update manifest file
# if absolute path is needed for renv profile
# Sys.setenv(RENV_PROFILE = "full")
# renv::paths$lockfile() # test
# assignInNamespace(
#   "renvLockFile",
#   \(...) renv::paths$lockfile(),
#   "rsconnect"
# )
# rsconnect::writeManifest() # run if needed
clinsight::run_app() # add parameters here (if any)
