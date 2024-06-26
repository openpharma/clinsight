#' Run the Shiny Application
#'
#' @param meta A data frame containing metadata. See `details` below for data
#'   specification.
#' @param data Either a data frame or a character string with the path to the
#'   app data in .rds format. See `details` below for data.frame specification.
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
#'    `subjec_id` and `event_date`
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
#' - `form_id`: character, a unique identifier for forms.
#' - `form_repeat`: integer, 
#' - `edit_date_time`: datetime (POSIXct), 
#' - `db_update_time`: datetime (POSIXct), 
#' - `region`: character, 
#' - `day`: difftime num???
#' - `vis_day`: numeric,
#' - `vis_num`: numeric,
#' - `event_label`: character, 
#' - `item_name`:  character, 
#' - `item_type`:  character, 
#' - `item_group`:  character, 
#' - `item_value`:  character, 
#' - `item_unit`:  character, 
#' - `lower_lim`: numeric,
#' - `upper_lim`: numeric, 
#' - `significance`:  character, 
#' - `reason_notdone`:  character, 
#' 
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
