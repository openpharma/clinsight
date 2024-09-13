library(shinytest2)

describe(
  "initialize_credentials(). (only applicable for shinymanager deployments). 
    Feature 1 | As a user, I want to be able to create a credentials 
    database if there is none yet. The database should be accessible by a common admin 
    account and password combination, with the requirement set in the database that 
    the password needs to be changed after first login. ", 
  {
    it(
      "Scenario 1 | Existing credentials database. 
        Given an existing a database named 'credentials.db' containing a test data 
        frame, stored in a temporary folder, 
        I expect that no new database will be created,
        and that the test data frame within the existing database still exists 
        and is unchaged", 
      {
        testthat::skip_if_not_installed("shinymanager")
        db_name <- file.path(withr::local_tempdir(), "credentials.db")
        con <- get_db_connection(db_name)
        DBI::dbWriteTable(con, "test", data.frame(test = 1))
        initialize_credentials(
          credentials_db = db_name,
          credentials_pwd = "test_password"
        )
        expect_equal(DBI::dbListTables(con), "test")
        temp_tbl <- dplyr::tbl(con, "test") |> dplyr::collect()
        expect_equal(temp_tbl, dplyr::tibble(test = 1))
      }
    )
    it(
      "Scenario 2 | Create a new credentials database. 
        Given that [credentials_db] leads to a non-existing database, 
        and [credentials_pwd] is set to 'test_password', 
        I expect that a new credentials database will be created,
        with the columns [user], [password], [start], [expire], [admin], [name], 
        [mail], [role], and [sites], [is_hashed_password], 
        and with the only existing [user] being 'admin',
        and the user named 'admin' having [admin] set to 'TRUE',
        and the [password] being hashed with the hased vector being different 
        from the initialization password ('1234'),
        and that the value (password) [must_change] is set to TRUE.", 
      {
        testthat::skip_if_not_installed("shinymanager")
        db_name <- file.path(withr::local_tempdir(), "credentials.sqlite")
        initialize_credentials(
          credentials_db = db_name,
          credentials_pwd = "test_password"
        )
        expect_true(file.exists(db_name))
        con <- get_db_connection(db_name)
        
        credentials_table <- shinymanager::read_db_decrypt(
          con, name = "credentials", passphrase = "test_password"
        )
        expect_equal(
          names(credentials_table),
          c("user", "password", "start", "expire", "admin", "name", 
            "mail", "roles", "sites", "is_hashed_password")
        )
        expect_equal(credentials_table[["user"]], "admin")
        expect_equal(credentials_table[["admin"]], "TRUE")
        # password is hashed: 
        expect_true(is.character(credentials_table$password))
        expect_true(credentials_table$password != "test_password")
        
        password_management_table <- shinymanager::read_db_decrypt(
          con, name = "pwd_mngt", passphrase = "test_password"
        )
        expect_equal(
          with(password_management_table, must_change[user == "admin"]),
          "TRUE"
        )
      }
    )
    it(
      "Scenario 3 | Create a new credentials database in a non-existent folder. 
        Given that [credentials_db] leads to a non-existing .sqlite database in 
        a nont-existent folder, 
        and [credentials_pwd] is set to 'test_password', 
        I expect that a new credentials database will be created in the specified folder.", 
      {
        testthat::skip_if_not_installed("shinymanager")
        db_name <- file.path(withr::local_tempdir(), "non_existing_folder/credentials.sqlite")
        initialize_credentials(
            credentials_db = db_name,
            credentials_pwd = "test_password"
          )
        expect_true(file.exists(db_name))
      }
    )
  }
)


describe(
  "authenticate_ui() and authenticate_server(). (only applicable for shinymanager 
  deployments). Feature 1 | As a user, I want to be able to 
  enter user name and password at the login screen of the application. 
  If the user name and password are incorrect, I want to see an error message, 
  and remain on the login screen.", 
  {
    it(
      "Scenario 1 | Access restricted. 
        Given a credentials database containing the user 'test_user_normal' 
        with password '1234', and the [credentials_db] does not yet exist, 
        and [credentials_pwd] is set to '1234', 
        and [credentials_folder] is set to an existing folder,
        and I start the application,
        I expect that I can only see the login screen,
        and that, when I try to log in with an incorrect password, I will not be 
        granted access.", 
      {
        testthat::skip_if_not_installed("shinymanager")
        app <- AppDriver$new(
          app_dir = test_path("fixtures/testapp-authentication"),
          name = "authenticate",
          timeout = 25000,
          width = 1619, 
          height = 955    
        )
        withr::defer(app$stop())
        app$wait_for_idle(800)
        app$expect_values(input = TRUE, output = TRUE)
        # 01. Incorrect login:
        app$set_inputs(
          "auth-user_id" = "test_user_normal",
          "auth-user_pwd" = "incorrect_pwd"
        )
        app$click("auth-go_auth")
        app$wait_for_idle()
        app$expect_values(input = TRUE, output = TRUE)
        expect_null(app$get_value(export = "user_error"))
        
        # After login, connection with the shiny app is lost in shinytest2, and 
        # therefore further automated tests dont work. Dont know how to solve this at the moment. 
        # # 02. Correct login
        # app$set_inputs(
        #   "auth-user_id" = "test_user_normal",
        #   "auth-user_pwd" = "1234"
        # )
        # app$click("auth-go_auth")
        # app$wait_for_idle(800)
        # app$expect_values(input = TRUE, output = TRUE)
        # # app$get_values(input = TRUE)
        # # app$set_inputs(
        # #   "password-pwd_one" = "TEST-password01",
        # #   "password-pwd_two" = "TEST-password01"
        # # )
        # # app$click("password-update_pwd")
        # # app$get_values(input = TRUE)
        # # app$set_inputs(main_tabs = "Study data")
      }
    )
  }
)
