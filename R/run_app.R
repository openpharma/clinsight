#' Run the Shiny Application
#'
#' @param meta A data frame containing metadata.
#' @param data Either a data frame or a character string with the path to the
#'   app data in .rds format.
#' @param user_db Character string. Path to the app database. If not existing,
#'   will be created based on app data and metadata, with all data labeled as
#'   'new'/not yet reviewed.
#' @param credentials_db Character string. Path to the credentials database.
#' @param credentials_pwd Character string with the credentials' database
#'   password.
#' @param test_mode Logical, whether to run the application in test mode.
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' 
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    meta = metadata, 
    data = clinsightful_data,
    user_db = "user_db.sqlite",
    credentials_db = "credentials_db.sqlite",
    credentials_pwd = Sys.getenv("DB_SECRET"),
    test_mode = FALSE,
    ...
) {
  if(is.character(data)){
    stopifnot("Data file does not exist." = file.exists(data))
    stopifnot(
      "Invalid data format. Expecting a file .rds format" = grepl("rds$", tolower(basename(data)))
    )
  } else{
    stopifnot("Invalid data format. Expecting a data frame." = is.data.frame(data) )
  }
  stopifnot("User database directory does not exist" = dir.exists(dirname(user_db)))
  stopifnot("metadata should be provided in list format" = inherits(meta, "list"))
  
  shinymanager::set_labels(
    language = "en",
    "Please authenticate" = "Login to continue"
  )
  options("shinymanager.pwd_validity" = 90) 
  options("shinymanager.pwd_failure_limit" = 5)
  
  if(!test_mode){
    stopifnot("Credentials database directory does not exist" = dir.exists(dirname(credentials_db)))
    stopifnot("No valid credentials database pwd provided" = is.character(credentials_pwd))
    if(nchar(credentials_pwd) == 0 ) stop("credentials_pwd cannot be blank it test_mode is FALSE")
    initialize_credentials(
      credentials_db = credentials_db,
      credentials_pwd = credentials_pwd
    )
  }
  
  with_golem_options(
    app = shinyApp(
      ui =  authenticate_ui(test_mode = test_mode),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      meta = meta,
      data = data,
      user_db = user_db,
      credentials_db = credentials_db, 
      credentials_pwd = credentials_pwd,
      test_mode = test_mode,
      ...
    )
  )
}
