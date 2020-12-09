#' Internal function to build table with `data.frame` with other arguments as
#' attributes. Useful for testing.
#'
#' @inheritParams factory_gt
#' @keywords internal
#' @return data.frame
factory_dataframe <- function(tab,
                              align = NULL,
                              hrule = NULL,
                              notes = NULL,
                              output_file = NULL,
                              output_format = NULL,
                              add_rows = NULL,
                              title = NULL,
                              ...) {

  out <- tab

  # factor -> character (useful for R<4.0.0)
  for (i in seq_along(out)) {
    if (is.factor(out[[i]])) {
      out[[i]] <- as.character(out[[i]])
    }
  }

  attr(out, 'align') <- align
  attr(out, 'hrule') <- hrule
  attr(out, 'notes') <- notes
  attr(out, 'output_file') <- output_file
  attr(out, 'output_format') <- output_format
  attr(out, 'title') <- title

  row.names(out) <- NULL

  return(out)

}
