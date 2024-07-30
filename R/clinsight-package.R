#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom htmltools htmlEscape
#' @importFrom rlang :=
#' @importFrom dplyr .data
#' @import shiny
#' @importFrom shiny NS tagList shinyApp
#' @importFrom stats na.omit setNames
#' @importFrom golem with_golem_options
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom purrr map2_df map
## usethis namespace: end
NULL


#' Dummy function
#' 
#' To remove note for R CMD check
#'
#' @noRd
dummy_function <- function(){
  dbplyr::as.sql()
}