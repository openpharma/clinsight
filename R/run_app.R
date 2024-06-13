#' Run the Shiny Application
#'
#' @param data_folder Character string. The folder in which all data resides is
#'   usually set in the config.yml file. However, this can be overwritten if a
#'   path is set in this argument. Useful for testing purposes.
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
    data_folder = NULL,
    credentials_pwd = Sys.getenv("DB_SECRET"),
    test_mode = FALSE,
    ...
) {
  data_folder <- data_folder %||% get_golem_config("data_folder") %||% "."
  if(!dir.exists(data_folder)){ dir.create(data_folder) }
  
  data <- get_golem_config("study_data")
  meta <- get_golem_config("meta_data")
  user_db <- file.path(data_folder, get_golem_config("user_db"))
  credentials_db <- file.path(data_folder, get_golem_config("credentials_db"))
  
  ## Verify study data
  if(is.character(data)){
    if(!dir.exists(data_folder)){
      stop("Folder '", data_folder, "' does not exist.\n")
    }
    data_path <- file.path(data_folder, data)
    if(!file.exists(data_path)) stop(paste0("Cannot find '", data_path, "'."))
    stopifnot(
      "Invalid data format. Expecting a file .rds format" =  
        tolower(tools::file_ext(data)) == "rds"
    )
    data <- readRDS(data_path)
  } 
  stopifnot("Expecting study data to be in data frame format." = is.data.frame(data) )
  ### TODO: Add tests to check if data is in correct format. Stop if this is not the case.
  
  ## Verify metadata
  if(is.character(meta)){
    meta_path <- file.path(data_folder, meta)
    meta_type <- tolower(tools::file_ext(meta))
    rds_path <- paste0(tools::file_path_sans_ext(meta_path), ".rds")
    if(!file.exists(meta_path) & !file.exists(rds_path)) {
      stop(paste0("Cannot find metadata file '", meta_path, "'."))
    }
    stopifnot("For metadata, only .xlsx or .rds format is allowed" = meta_type %in% c("rds", "xlsx"))
    if(meta_type == "xlsx" & !file.exists(rds_path)){
        cat("Reading metadata and creating a new .rds file\n")
        meta <- get_metadata(meta_path)
        saveRDS(meta, rds_path)
    } else if(meta_type == "xlsx"){
      cat("Reading metadata from '", rds_path, "'. Delete this .rds file if '", 
          meta, "' should be used instead.\n", sep = "")
    }
    meta <- readRDS(rds_path)
  }
  stopifnot("Expecting metadata to be in a list format" = inherits(meta, "list"))
  
  ## Verify user database
  if(!file.exists(user_db)){
    warning("No user database found. New database will be created")
    db_create(get_review_data(data), db_path = user_db)
  } else{
    if(!test_mode){
      db_update(get_review_data(data), db_path = user_db, data_synched = FALSE) 
    }
  }
  
  ## Verify credentials database, if applicable
  if(!test_mode){
    stopifnot("Credentials database directory does not exist" = dir.exists(dirname(credentials_db)))
    stopifnot("No valid credentials database pwd provided" = is.character(credentials_pwd))
    if(nchar(credentials_pwd) == 0 ) stop("credentials_pwd cannot be blank it test_mode is FALSE")
    initialize_credentials(
      credentials_db = credentials_db,
      credentials_pwd = credentials_pwd
    )
  }
  
  shinymanager::set_labels(
    language = "en",
    "Please authenticate" = "Login to continue"
  )
  options("shinymanager.pwd_validity" = 90) 
  options("shinymanager.pwd_failure_limit" = 5)
  
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
