#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "clinsight")
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE. If unset,
#'   "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file. Can be set with the option
#'   'CONFIG_PATH' so that a study-specific config.yml file can be provided at
#'   runtime, without rebuilding the application's package from source.
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv(
      "R_CONFIG_ACTIVE",
      "default"
    )
  ),
  use_parent = TRUE,
 file = custom_config_path()
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
