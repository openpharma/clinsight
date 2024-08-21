#' Get roles from config
#'
#' Helper function to retrieve all roles from the config file.
#'
#' @param config_roles Character string with the name of the config argument to
#'   read from.
#'
#' @return A named character vector.
#' @noRd
get_roles_from_config <- function(
    config_roles = "user_roles"
){
  stopifnot(is.character(config_roles))
  all_roles <- unlist(get_golem_config(config_roles))
  names(all_roles) <- names(all_roles) %||% all_roles
  all_roles
}


#' Get valid roles
#'
#' Helper function to retrieve all roles that are relevant for the application.
#'
#' @param roles A character vector. Will be converted to lower case.
#'
#' @return A named character vector with the roles applicable for the
#'   application and available for the current user.
#' 
get_valid_roles <- function(
    roles
){
  stopifnot(is.character(roles))
  all_roles <- get_roles_from_config()
  
  roles <- trimws(unlist(strsplit(tolower(roles), ",")))
  all_roles[sort(match(roles, all_roles))]
}
