#' Filter app data
#'
#' @param data A `Reactivevalues` object. filtered data will be written into
#'   this object.
#' @param sites Character vector with sites to filter on.
#' @param subject_ids Character vector with all subject IDs. Used to keep the
#'   correct order of subject IDs.
#' @param appdata Application data in long format, stored in a list. List
#'   contains data frames named per form.
#' @param apptables Application data tables in wide format, stored in a list.
#'   List contains data frames named per form.
#'
#' @return A `reactivevalues` object.
#' 
filter_data <- function(
    data, 
    sites, 
    subject_ids, 
    appdata, 
    apptables
    ){
  stopifnot(is.reactivevalues(data))
  
  data$filtered_data <- lapply(appdata, \(x){
    with(x, x[site_code %in% sites, ])
  })
  filtered_ids <- unique(bind_rows_custom(data$filtered_data)$subject_id)
  # To ensure the right order of IDs:
  data$filtered_subjects <- subject_ids[subject_ids %in% filtered_ids]
  cat("selected subjects: ", data$filtered_subjects, "\n\n")
  data$filtered_tables <- lapply(apptables, \(x){
    with(x, x[subject_id %in% data$filtered_subjects, ] )
  })
  data$subject_id <- data$filtered_subjects[1]
  golem::cat_dev("Finished applying review configuration\n\n")
  data
}

#' Set user role
#'
#' Sets user role in a `reactiveValues()` object. Designed to be used within
#' [mod_review_config_server()].
#'
#' @param user_role Character vector with the selected user role.
#'
#' @inheritParams filter_data
#' 
set_user_role <- function(
    data,
    user_role
){
  stopifnot(is.reactivevalues(data))
  user_role <- user_role %||% ""
  stopifnot(is.character(user_role))
  if(!isTruthy(isolate(data$user_role))){
    data$user_role <- reactive("")  
  }
  if(!identical(isolate(data$user_role()), user_role)){
    data$user_role <- reactive({user_role}) 
    golem::cat_dev("Finished applying review configuration\n\n")  
  }
  data
}
