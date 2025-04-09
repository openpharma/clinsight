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
  if (
    clinsight_config == "default" & 
    !identical(clinsight_data, clinsightful_data) & 
    !identical(meta_data, metadata)
  ){
    stop("The 'default' config should not be used with custom data.")
  }
  
  temp_folder <- tempfile(tmpdir = tempdir())
  dir.create(temp_folder, recursive = TRUE)
  saveRDS(clinsight_data, file.path(temp_folder, "study_data.rds"))
  saveRDS(meta_data, file.path(temp_folder, "metadata.rds"))
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