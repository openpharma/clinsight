#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' If x does not exist, return y, otherwise return x
#' 
#' @param x,y two elements to test, one potentially not existent
#' @param verbose logical, indicating whether warning message should be displayed.
#' 
#' @noRd
#' 
#' @examples
#' mtcars2 %|_|% mtcars
"%|_|%" <- function(x, y, verbose = TRUE) {
  if (exists(deparse1(substitute(x)), envir = parent.frame())) {
    if (verbose) cat("Using user supplied", deparse(deparse1(substitute(x))), "instead of deriving.\n")
    x
  } else {
    y
  }
}
