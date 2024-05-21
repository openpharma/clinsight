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
withr::with_options(new = list("shiny.testmode" = TRUE), {
  run_app(data_path = app_sys("app/www/raw_data.rds"), database_path = app_sys("app/www", "user_database.db"), test_mode = TRUE)
})
