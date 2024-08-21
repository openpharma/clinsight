#' Initialize database
#'
#' Creates a database with new credentials if it does not yet exist.
#'
#' @param credentials_db Character string. Path to the database.
#' @param credentials_pwd Password for the credentials database.
#'
#' @return Creates credentials database if needed. No values will be returned.
#' @export
#' 
initialize_credentials <- function(
    credentials_db = "credentials_db.sqlite",
    credentials_pwd = Sys.getenv("DB_SECRET")
){
  if(file.exists(credentials_db)) return(
    cat("Using existing credentials database.\n")
  )
  
  cat("No credentials database found. Initializing new database.\n", 
      "Login with Username 'admin' and Password '1234'.\n")
  cred_directory <- dirname(credentials_db)
  if(tools::file_ext(credentials_db) == "sqlite" && !dir.exists(cred_directory)) {
    cat("Folder to store credentials database does not exist. ", 
        "Creating new directory named '", cred_directory, "'.\n", sep = "")
    dir_created <- dir.create(cred_directory)
    if(!dir_created) stop("Could not create directory for user database")
  }
  
  con <- get_db_connection(credentials_db)
  initial_credentials <- data.frame(
    "user"     = "admin", 
    "password" = "1234",
    "start"    = Sys.Date(), 
    "admin"    = TRUE,
    "name"     = "Admin",
    "mail"     = "", 
    "roles"    = names(get_roles_from_config())[1], 
    "sites"    = "",
    stringsAsFactors = FALSE, 
    check.names = FALSE
  )
  
  shinymanager::create_db(
    initial_credentials,
    credentials_db,
    passphrase = credentials_pwd
  )
  
  # New users should always change their password: 
  password_management_table <- shinymanager::read_db_decrypt(
    con, name = "pwd_mngt",
    passphrase = credentials_pwd
  ) |> 
    dplyr::mutate(
      must_change = ifelse(
        have_changed == "TRUE", must_change, as.character(TRUE)
      )
    )
  shinymanager::write_db_encrypt(
    con,
    value = password_management_table,
    name = "pwd_mngt",
    passphrase = credentials_pwd
  )
}

#' Authenticate UI
#'
#' Authentication implementation in the UI, using `shinymanager`.
#'
#' 
authenticate_ui <- function(){
  shinymanager::secure_app(
    app_ui, 
    enable_admin = TRUE, 
    theme = bslib::bs_theme(bootswatch = "spacelab", version = "5"),
    tags_top = tags$div(
      golem_add_external_resources(),
      tags$img(src='www/gcp_logo.png', height = '100', width ='450'),
      HTML("<br><br>")
    ),
    tags_bottom = tags$div(
      tags$p(
        "For any question, please  contact the ",
        tags$a(
          href = "mailto:lsamson@gcp-service.com?Subject=ClinSight",
          target="_top", "administrator"
        )
      )
    ),
    background = "background-color: #454545;"
  )
}

#' Authenticate server
#'
#' Function to authenticate the main server.
#'
#' @param user_identification Character vector showing the user identification.
#'   Is by default set by the `user_identification` option in the `golem-config`
#'   file.
#' @param credentials_db Character vector. Path to the credentials database. By
#'   default, set by the `data_folder` and `credentials_db` options in the
#'   `golem-config_file`.
#' @param credentials_pwd Character vector, containing the database password.
#' @param all_sites Character vector with all sites. Will be passed on to
#'   shinymanager configuration so that data can be restricted to specific sites
#'   per user.
#' @param all_roles Named character vector. Used to show all roles that are
#'   valid to use in the application. Also used to show all applicable roles
#'   in `shinymanager` admin mode.
#' @param session Shiny session. Needed to access user information in case of
#'   login methods alternative to `shinymanager` are used.
#' 
authenticate_server <- function(
    user_identification = get_golem_config("user_identification"),
    all_sites = NULL,
    all_roles = get_roles_from_config(),
    credentials_db = get_golem_config("credentials_db"),
    credentials_pwd = Sys.getenv("DB_SECRET"),
    session
){
  switch(
    user_identification,
    shinymanager = shinymanager::secure_server(
      check_credentials = shinymanager::check_credentials(
        credentials_db,
        passphrase = credentials_pwd
      ),
      inputs_list = list(
        "roles" = list(
          fun = "selectInput", 
          args = list(choices = names(all_roles), multiple = TRUE) 
        ),
        "sites" = list(
          fun = "selectInput",
          args = list(
            label = NULL, 
            choices = all_sites, 
            selected = all_sites, 
            multiple = TRUE
          )
        )
      )
    ),
    test_user = reactiveValues(
      user = "test_user", 
      name = "test user", 
      roles = all_roles[1:3],
      sites = all_sites
    ),
    http_headers = reactiveValues(
      user = session$request$HTTP_X_SP_USERID,
      name = session$request$HTTP_X_SP_USERNAME,
      roles = get_valid_roles(session$request$HTTP_X_SP_USERGROUPS, all_roles),
      sites = all_sites
    ),
    shiny_session = reactiveValues(
      user = session$user,
      name = session$user,
      roles = get_valid_roles(session$groups, all_roles),
      sites = all_sites
    ),
    reactiveValues(
      user = "Unknown",
      name = "Unknown",
      roles = "",
      sites = all_sites
    )
  )
}
