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
#' @param all_roles A named character vector with all applicable roles.
#'
#' @return A named character vector with the roles applicable for the
#'   application and available for the current user.
#' 
get_valid_roles <- function(
    roles,
    all_roles = get_roles_from_config()
){
  if(is.null(roles)){
    warning("No roles found. Is the active configuration correct?")
    roles <- ""
  }
  stopifnot(is.character(roles) || is.list(roles), is.character(all_roles))
  roles <- trimws(unlist(strsplit(tolower(roles), ",")))
  all_roles[sort(match(roles, all_roles))]
}
