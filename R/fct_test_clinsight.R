#' Test ClinSight
#'
#' Provides a wrapper around [run_app()]. Unlike the function [run_app()], this
#' function stores data in a temporary folder and cleans up after itself after
#' use. Useful during development, when tweaking new study data and metadata for
#' use with ClinSight. Note that, when using this function, any database files
#' needed for ClinSight will be renewed and any file path specified in the
#' `config.yml` file will be ignored.
#'
#' This function is only meant for testing and development purposes. For use in
#' production, instead use the function [run_app()] directly.
#'
#' @param clinsight_data A data frame with (ClinSight compatible) study data. If
#'   not provided, the example study data in the ClinSight package will be used.
#' @param meta_data A list of data frames with (ClinSight compatible) metadata.
#'   If not provided, the example metadata in the ClinSight package will be used
#' @param clinsight_config A character vector with the ClinSight configuration
#'   to use. The standard settings should be sufficient for most testing cases.
#'
#' @return Runs the ClinSight Shiny application.
#' @export
#' 
test_clinsight <- function(
    clinsight_data = clinsightful_data, 
    meta_data = metadata,
    clinsight_config = "test"
){
  stopifnot("Expecting a data frame" = is.data.frame(clinsight_data))
  stopifnot("Expecting a list" = inherits(meta_data, "list"))
  stopifnot(is.character(clinsight_config))
  study_data_path <- get_golem_config("study_data", config = clinsight_config)
  meta_data_path <- get_golem_config("meta_data", config = clinsight_config)
  if (
    clinsight_config %in% c("default", "dev") | 
    !is.character(study_data_path) |
    !is.character(meta_data_path) #|
  ){
    stop("The 'default' or 'dev' config cannot be used with custom data, ", 
         "and meta_data, app_data, app_vars, app_tables, & available_data ", 
         "in the config file should be character vectors.")
  }
  # Build a version of `app_data` & app_vars
  app_data <- get_appdata(data = clinsight_data, meta = meta_data)
  app_vars <- get_meta_vars(data = app_data, meta = meta_data)

  # Build a 'app_tables'
  app_tables <- lapply(
    setNames(names(app_data), names(app_data)), \(x){
      create_table(app_data[[x]], expected_columns = names(app_vars$items[[x]]))
    })

  # Build a 'available_data'
  available_data <- get_available_data(
    data = app_data,
    tables = app_tables,
    all_forms = app_vars$all_forms,
    form_repeat_name = with(
      meta[["table_names"]],
      table_name[raw_name == "form_repeat"]
    ) |>
      tryCatch(error = \(e) "N")
  )
  
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder, recursive = TRUE)
  # saveRDS(clinsight_data, file.path(temp_folder, basename(study_data_path))) 
  # saveRDS(meta_data, file.path(temp_folder, basename(meta_data_path)))
  db_path <- file.path(temp_folder, "user_db.sqlite")
  if(file.exists(db_path)) file.remove(db_path)
  db_create(get_review_data(clinsight_data),
            db_path = db_path
  )
  save_objs <- c(
    "clinsight_data",
    "meta_data",
    "app_data",
    "app_vars",
    "app_tables",
    "available_data")
  purrr::walk(save_objs, function(x){
    rds_file <- file.path(temp_folder, paste0(x, ".rds"))
    saveRDS(get(x), rds_file)
    if(inherits(get(x), "data.frame")) {
      pq_file <- file.path(temp_folder, paste0(x, ".parquet"))
      arrow::write_parquet(get(x), pq_file)
    }
  })
  old_config <- Sys.getenv("GOLEM_CONFIG_ACTIVE")
  Sys.setenv("GOLEM_CONFIG_ACTIVE" = clinsight_config)
  run_app(
    data_folder = temp_folder, 
    credentials_pwd = "TEMP_PASSWORD",
    onStart = \(){onStop(\(){
      unlink(temp_folder, recursive = TRUE);
      Sys.setenv("GOLEM_CONFIG_ACTIVE" = old_config)
    })}
  )
}