#' Run the Shiny Application
#'
#' @param data_folder Character string. The folder in which all data resides is
#'   usually set in the config.yml file. However, this can be overwritten if a
#'   path is set in this argument. If used, any path specified in the config.yml
#'   will be ignored. Useful for testing purposes.
#' @param credentials_pwd Character string with the credentials' database
#'   password.
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
#'
#'
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
    ...
) {
  
  # data <- get_golem_config("study_data")
  meta <- get_golem_config("meta_data")
  app_data <- get_golem_config("app_data")
  app_vars <- get_golem_config("app_vars")
  # app_tables <- get_golem_config("app_tables")
  available_data <- get_golem_config("available_data")
  timeline_data <- get_golem_config("timeline_data")
  user_db <- get_golem_config("user_db")
  use_shinymanager <- isTRUE(get_golem_config("user_identification") == "shinymanager")
  credentials_db <- get_golem_config("credentials_db")
  
  if(!is.null(data_folder)){
    if(!dir.exists(data_folder)) dir.create(data_folder) 
    if(!dir.exists(data_folder)){
      stop("Folder path '", data_folder, "' specified but cannot be created\n")
    }
    golem::cat_dev(
      "Custom folder path provided in the 'data_folder' argument.",
      "File paths specified in the config.yml will be ignored."
    )
    # if(is.character(data)) data <- file.path(data_folder, basename(data))
    if(is.character(meta)) meta <- file.path(data_folder, basename(meta))
    if(is.character(app_data)) app_data <- file.path(data_folder, basename(app_data))
    if(is.character(app_vars)) app_vars <- file.path(data_folder, basename(app_vars))
    # if(is.character(app_tables)) app_tables <- file.path(data_folder, basename(app_tables))
    if(is.character(available_data)) available_data <- file.path(data_folder, basename(available_data))
    if(is.character(timeline_data)) timeline_data <- file.path(data_folder, basename(timeline_data))
    user_db <-  file.path(data_folder, basename(user_db))
    if(!is.null(credentials_db)){
      credentials_db <- file.path(data_folder, basename(credentials_db)) 
    }
  }
  
  # ## Verify study data
  # if(is.character(data)){
  #   if(!file.exists(data)) stop(paste0("Cannot find '", data, "'."))
  #   if(tolower(tools::file_ext(data)) != "rds"){
  #     stop("Invalid data format. Expecting a file .rds format")
  #   }
  #   data <- readRDS(data)
  # } 
  # stopifnot("Expecting study data to be in data frame format." = is.data.frame(data))
  
  
  ## Verify app_data list
  if(is.character(app_data)){
    if(!file.exists(app_data)) stop(paste0("Cannot find '", app_data, "'."))
    if(tolower(tools::file_ext(app_data)) != "rds"){
      stop("Invalid 'app_data' format. Expecting a file .rds format")
    }
    app_data <- readRDS(app_data)
  } 
  stopifnot("Expecting 'app_data' to be in list format." = inherits(app_data, "list"))
  
  
  ## Verify app_vars list
  if(is.character(app_vars)){
    if(!file.exists(app_vars)) stop(paste0("Cannot find '", app_vars, "'."))
    if(tolower(tools::file_ext(app_vars)) != "rds"){
      stop("Invalid 'app_vars' format. Expecting a file .rds format")
    }
    app_vars <- readRDS(app_vars)
  } 
  stopifnot("Expecting 'app_vars' to be in list format." = inherits(app_vars, "list"))
  
  
  ## Verify metadata
  if(is.character(meta)){
    if(!file.exists(meta)) stop(paste0("Cannot find metadata ('", meta, "')."))
    if(tolower(tools::file_ext(meta)) != "rds") {
      stop("Only metadata files of type '.rds' are allowed.")
    }
    meta <- readRDS(meta)
  }
  stopifnot("Expecting 'metadata' to be in a list format" = inherits(meta, "list"))
  
  
  ## Verify app_tables list
  # if(is.character(app_tables)){
  #   if(!file.exists(app_tables)) stop(paste0("Cannot find '", app_tables, "'."))
  #   if(tolower(tools::file_ext(app_tables)) != "rds"){
  #     stop("Invalid 'app_tables' format. Expecting a file .rds format")
  #   }
  #   app_tables <- readRDS(app_tables)
  # } 
  # stopifnot("Expecting 'app_tables' to be in list format." = inherits(app_tables, "list"))
  
  
  ## Verify available_data
  if(is.character(available_data)){
    if(!file.exists(available_data)) stop(paste0("Cannot find '", available_data, "'."))
    available_data <-
      switch(
        tolower(tools::file_ext(available_data)),
        "rds" = readRDS(available_data),
        "parquet" = arrow::read_parquet(available_data),
        stop("Invalid 'available_data' format. Expecting an RDS or Parquet file.")
      )
  } 
  stopifnot("Expecting 'available_data' to be in data frame format." = is.data.frame(available_data))
  
  
  ## Verify timeline_data
  if(is.character(timeline_data)){
    if(!file.exists(timeline_data)) stop(paste0("Cannot find '", timeline_data, "'."))
    timeline_data <-
      switch(
        tolower(tools::file_ext(timeline_data)),
        "rds" = readRDS(timeline_data),
        "parquet" = arrow::read_parquet(timeline_data),
        stop("Invalid 'timeline_data' format. Expecting an RDS or Parquet file.")
      )
  } 
  stopifnot("Expecting 'timeline_data' to be in data frame format." = is.data.frame(timeline_data))
  
  
  ## Verify user database
  stopifnot("user_db should be a character vector with a file path" = 
              is.character(user_db))
  if(!file.exists(user_db)){
    warning("No user database found. New database will be created")
    db_create(get_review_data(data), db_path = user_db)
  } else{
    stopifnot("user_db version is not up to date" =
                identical(db_version, db_get_version(user_db)))
    # Skip if not needed for faster testing:
    if(isTRUE(get_golem_config("app_prod"))){
      db_update(get_review_data(data), db_path = user_db) 
    }
  }
  
  ## Verify credentials database, if applicable
  if(use_shinymanager){
    rlang::check_installed(
      "shinymanager", 
      reason = "to use it for authentication management in ClinSight."
      )
    stopifnot("credentials_db should be a character vector with a file path" = 
                is.character(credentials_db))
    stopifnot("No valid credentials database pwd provided" = is.character(credentials_pwd))
    if(nchar(credentials_pwd) == 0 ) stop("credentials_pwd cannot be blank when using shinymanager")
    initialize_credentials(
      credentials_db = credentials_db,
      credentials_pwd = credentials_pwd
    )
    shinymanager::set_labels(
      language = "en",
      "Please authenticate" = "Login to continue"
    )
    options("shinymanager.pwd_validity" = 90) 
    options("shinymanager.pwd_failure_limit" = 5)
  }
  logo_path <- get_golem_config("study_logo")
  study_logo_path <- if (file.exists(logo_path)){
    if(!tolower(tools::file_ext(logo_path)) %in% c("png", "jpg", "svg")){
      warning("study logo ignored - only png, jpg or svg files are supported.")
      return(NULL)
    }
    paste0("assets/", basename(logo_path))
  } else{
    NULL
  }
  
  with_golem_options(
    app = shinyApp(
      ui =  if(use_shinymanager) authenticate_ui() else app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      meta = meta,
      app_data = app_data,
      app_vars = app_vars,
      # app_tables = app_tables,
      available_data = available_data,
      timeline_data = timeline_data,
      data = data,
      user_db = user_db,
      credentials_db = credentials_db,
      credentials_pwd = credentials_pwd,
      study_logo_path = study_logo_path,
      ...
    )
  )
}
