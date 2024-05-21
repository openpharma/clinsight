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
          "Login with Username 'admin' and Password '1234'.")
  con <- get_db_connection(credentials_db)
  initial_credentials <- data.frame(
    "user"     = "admin", 
    "password" = "1234",
    "start"    = Sys.Date(), 
    "admin"    = TRUE,
    "name"     = "Admin",
    "mail"     = "", 
    "role"     = "", 
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
#' Authentication implementation in the UI.
#'
#' @param test_mode Logical. Whether the app should be started in test mode or
#'   not.
#'
#' 
authenticate_ui <- function(test_mode = FALSE){
  if (test_mode) return(app_ui) 
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
#' @param test_mode Logical, whether to start the application in test mode.
#' @param sites Character vector. Study sites that can be allocated to a user.
#' @param roles Character vector. Roles that can be allocated to a user.
#' @param credentials_db Character vector. Path to the credentials
#'   database.
#' @param credentials_pwd Character vector, containing the database
#'   password.
#' 
authenticate_server <- function(
    test_mode = FALSE, 
    sites = app_vars$Sites$site_code,
    roles = c("Medical Monitor", "Data Manager", "Administrator", "Investigator"),
    credentials_db = app_sys("app/www/credentials_db.sqlite"),
    credentials_pwd = Sys.getenv("DB_SECRET")
){
  if (test_mode) return({
    # To skip authentication when testing application:
    reactiveValues(
      admin = TRUE,
      user = "test_user",
      name = "test user", 
      role = "Medical monitor",
      sites = sites
    )
  }) 
  shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      credentials_db,
      passphrase = credentials_pwd
    ),
    inputs_list = list(
      "role" = list(
        fun = "selectInput", 
        args = list(choices = roles, multiple = TRUE) 
      ),
      "sites" = list(
        fun = "selectInput",
        args = list(label = NULL, choices = sites, selected = sites, multiple = TRUE)
      )
    )
  ) 
}
