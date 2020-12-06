#' Internal function to prepare stars
#'
#' @keywords internal
clean_stars <- function(stars) {
  if (is.logical(stars)) {
    if (stars) {
      stars <- c('*' = .1, '**' = .05, '***' = .01)
    }
  }
  if (is.numeric(stars)) {
    out <- sort(stars, decreasing = TRUE)
  } else {
    out <- NULL
  }
  return(out)
}


#' Internal function to prepare stars
#'
#' @keywords internal
make_stars <- function(pvalues, stars) {
  stars <- clean_stars(stars)
  tmp <- ""
  for (n in names(stars)) {
    tmp <- ifelse(pvalues <= stars[n], n, tmp)
  }
  tmp
}


#' Internal function to prepare stars footnote
#'
#' @keywords internal
make_stars_note <- function(stars) {
  out <- clean_stars(stars)
  if (!is.null(out)) {
    out <- paste0(names(out), ' p < ', out)
    out <- paste0(out, collapse = ', ')
  }
  return(out)
}
