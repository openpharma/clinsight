#' Run the Shiny Application
#' 
#' @param meta A data frame containing metadata. See `details` below for data
#'   specification.
#' @param data Either a data frame or a character string with the path to the
#'   app data in .rds format. See `details` below for data.frame specification.
#' @param data_folder Character string. The folder in which all data resides is
#'   usually set in the config.yml file. However, this can be overwritten if a
#'   path is set in this argument. Useful for testing purposes.
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
#' @details
#' Two of the arguments `meta` and `data` are crucial to successful app
#' deployment. As such, here are comprehensive data specifications for these
#' objects:
#' 
#' Column specs for the `data` object:
#' - `site_code`: character or integer, identifier for study site; If an integer,
#'    recommended to add prefix "Site" as this will display more intuitively in
#'    the application's UI
#' - `subject_id`: character, unique identifier for a subject
#' - `event_repeat`: integer, helps keep track of unique `event_id` for a single
#'    `subject_id` and `event_date`
#' - `event_id`: character, names that help classify types of  `event_name`s
#'    into like-groups, generally characterized by site visits. For example, 
#'    "SCR" for the screening visit, "VIS" for Visit X (where X is some integer),
#'    and "EXIT" for when the patient exits the study trial. However, some
#'    `event_id`s track events that could apply outside of any visit, like AE,
#'    ConMed, Medical History, etc.
#' - `event_name`: character, an "event" generally characterizes some sort of
#'    site visit, whether that be a "Screening", "Visit X" (where X is some
#'    integer), "Exit", or "Any Visit".
#' - `event_date`: Date, the date associated with `event_name`
#' - `form_id`: character, a unique identifier for the form the `item_name` metric
#'    and `item_value` were pulled from. Note: when `item_type` is continuous,
#'    `form_id` can contain several different `item_group`s. However, when
#'    `item_type` is 'other', `item_group` can be made up of several `form_id`
#'    values.
#' - `form_repeat`: integer, helps keep track of unique `item_name`s collected
#'    from a specific `form_id` for a given `subject_id`. `form_repeat` is
#'    particularly helpful when conslidating data like Adverse Events into this
#'    data format. Specifically, if more than one AE is collected on a patient,
#'    they'll have more than one `form_repeat`
#' - `edit_date_time`: datetime (POSIXct), the last time this record was edited
#' - `db_update_time`: datetime (POSIXct), the last time the database storing this
#'    record was updated.
#' - `region`: character, describing the region code that `site_code` falls under
#' - `day`: a difftime number, meaning it contains both a number and unit of 
#'    time. It measures the number of days each visit is from screening
#' - `vis_day`: numeric, a numeric representation of `day`
#' - `vis_num`: numeric, a numeric representation of `event_name`
#' - `event_label`: character, an abbreviation of `event_name`
#' - `item_name`:  character, describes a metric or parameter of interest.
#' - `item_type`:  character, classifies `item_name`s into either 'continuous' 
#'    or 'other', where continuous types are those generally associated with the
#'    CDISC "basic data structure" (BDS). That is, each `item_name` metric is
#'    collected over time at a patient visit (`event_name`). The 'other' type
#'    represents all non-time dependent measures, like demographic info, adverse
#'    events, Medications, medical history, etc.
#' - `item_group`:  character, provides is a high level category that groups 
#'    like-`item_name`s together. For example, and `item_group` = 'Vital Signs'
#'    will group together pertinent `item_name` metrics like BMI, Pulse, Blood
#'    pressure, etc.
#' - `item_value`:  character, the measurement collected for a given `item_name`.
#'    The value collected may be a number like 150 (when collecting a patient's
#'    weight) or a word (such as 'white' for the subject's race).
#' - `item_unit`:  character, tracking the unit of measurement for `item_name` 
#'    and `item_value`.
#' - `lower_lim`: numeric, some `item_name`s (particularly the 'continuous' type)
#'    have a pre-defined range of values that are considered normal. This is the
#'    lower limit to that range.
#' - `upper_lim`: numeric, some `item_name`s (particularly the 'continuous' type)
#'    have a pre-defined range of values that are considered normal. This is the
#'    upper limit to that range.
#' - `significance`:  character, either 'CS' which means 'Clinically Significant'
#'    or 'NCS' which means 'Not Clinically Significant'
#' - `reason_notdone`:  character, an effort to describe why the `item_value`
#'    field is `NA` / missing.
#' 
#' 
#' Specifications for list items that may be included in the `meta` object:
#' 
#' `column_specs` a data.frame
#' 
#' `events` a data.frame
#' 
#' `common_forms` a data.frame
#' 
#' `study_forms` a data.frame
#' 
#' `general` a data.frame
#' 
#' `groups` a data.frame
#' 
#' `table_names` a data.frame
#' 
#' `items_expanded` a data.frame
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
    if(!file.exists(meta_path)) {
      stop(paste0("Cannot find metadata file '", meta_path, "'."))
    }
    if(tolower(tools::file_ext(meta)) != "rds") {
      stop("Only metadata files of type '.rds' are allowed.")
      }
    meta <- readRDS(meta_path)
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
