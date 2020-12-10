#' Allow users to override uncertainty estimates
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @keywords internal
#' @return a numeric vector of test statistics
extract_statistic_override <- function(model, statistic_override, conf_level=NULL) {

  # needed for logic tests
  out <- NULL

  # # sanity checks at the individual level
  # checkmate::assert(
  #   checkmate::check_function(statistic_override),
  #   checkmate::check_matrix(statistic_override),
  #   checkmate::check_atomic_vector(statistic_override, names="named"),
  #   combine = "or")

  if (is.function(statistic_override) || is.matrix(statistic_override)) {
    out <- get_coeftest(model, statistic_override, conf_level)
    if (inherits(out, "data.frame")) return(out)
  }

  # function is expected to return a matrix
  if (is.function(statistic_override)) {
    out <- try(statistic_override(model), silent=TRUE)
  } else {
    out <- statistic_override
  }

  if (is.matrix(out)) {
    out <- sqrt(base::diag(out))
    out <- data.frame(term=names(out), std.error=out)
  } 

  # atomic vector
  flag <- checkmate::check_atomic_vector(out, names="named")
  if (isTRUE(flag)) {
    out <- data.frame(term=names(out), 
                      std.error=out)
  }

  # factor -> character (important for R<4.0.0)
  for (i in seq_along(out)) {
    if (is.factor(out[[i]])) {
      out[[i]] <- as.character(out[[i]])
    }
  }
  
  return(out)

  stop("Could not retrieve a valid variance-covariance matrix using the function supplied in `statistic_override`.")

}


get_coeftest <- function(model, statistic_override, conf_level) {

  if (!check_dependency("lmtest")) return(NULL)

  gof <- try(
    lmtest::coeftest(model, vcov.=statistic_override), silent=TRUE)

  gof_ci <- try(
    lmtest::coefci(model, vcov.=statistic_override, level=conf_level),
    silent=TRUE)

  if (!inherits(gof, "try-error")) {
    gof <- as.data.frame(unclass(gof))
    colnames(gof) <- c("estimate", "std.error", "statistic", "p.value")
    gof$term <- row.names(gof)
  }

  if (!inherits(gof_ci, "try-error") &&
      !inherits(gof, "try-error")) {
    gof_ci <- as.data.frame(unclass(gof_ci))
    colnames(gof_ci) <- c("conf.low", "conf.high")
    gof_ci$term <- row.names(gof_ci)
    gof <- merge(gof, gof_ci, by="term")
  }

  if (inherits(gof, "try-error")) {
    gof <- NULL
  }

  return(gof)
}

