#' extract gof using the easystats `performance` package
#'
#' @keywords internal
glance_easystats <- function(model, ...) {
  if (!check_dependency("performance")) {
    return(NULL)
  }
  error_msg <- utils::capture.output(out <- performance::model_performance(model))

  # lm model: include F-stat by default
  if (isTRUE(class(model)[1] == "lm")) { # glm also inherits from lm
    out$F <- attr(out, "r2")$F
  }

  out <- insight::standardize_names(out, style="broom")
  mi <- insight::model_info(model)

  # nobs
  if ("n_obs" %in% names(mi)) {
    out$nobs <- mi$n_obs
  }

  # logLik
  ll <- try(stats::logLik(model), silent=TRUE)
  if (!inherits(ll, "try-error")) {
    out$logLik <- as.numeric(ll[1])
  }


  return(out)
}

#' extract estimates using the easystats `parameters` package
#'
#' @keywords internal
tidy_easystats <- function(model, ...) {
  if (!check_dependency("parameters")) {
    return(NULL)
  }
  msg <- utils::capture.output(out <- parameters::model_parameters(model, ...))
  out <- parameters::standardize_names(out, style="broom")
  return(out)
}
