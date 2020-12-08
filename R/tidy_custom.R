#' Extract custom information from a model object and turn it into a tidy
#' data.frame or tibble
#'
#' @param x An object to be converted into a tidy data.frame or tibble.
#' @return A data.frame or tibble with information about model components.
#' @keywords internal
#' @export
tidy_custom <- function(x) {
  UseMethod("tidy_custom")
}

#' @inherit tidy_custom
#' @export
tidy_custom.default <- function(x) NULL
