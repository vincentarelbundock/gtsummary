#' Extract model gof A mostly internal function with some potential uses
#' outside.
#'
#' @inheritParams get_estimates
#' @param vcov_type string vcov type to add at the bottom of the table
#' @export
get_gof <- function(model, vcov_type = NULL, ...) {

    # priority
    get_priority <- getOption("modelsummary_get", default = "broom")
    checkmate::assert_choice(
      get_priority,
      choices = c("broom", "easystats", "parameters", "performance", "all"))

    if (get_priority %in% c("all", "broom")) {
        funs <- list(get_gof_broom, get_gof_parameters)
    } else {
        funs <- list(get_gof_parameters, get_gof_broom)
    }

    warning_msg <- NULL

    gof <- NULL

    for (f in funs) {
        if (get_priority == "all") {
            tmp <- f(model, ...)
            if (inherits(tmp, "data.frame") &&
                inherits(gof, "data.frame")) {
                idx <- !tolower(colnames(tmp)) %in% tolower(colnames(gof))
                tmp <- tmp[, idx, drop = FALSE]
                if (ncol(tmp) > 0) {
                    gof <- bind_cols(gof, tmp)
                }
            } else if (inherits(tmp, "data.frame")) {
                gof <- tmp
            } else {
                warning_msg <- c(warning_msg, tmp)
            }

        } else {
            if (!inherits(gof, "data.frame")) {
                gof <- f(model, ...)
            }
        }
    }

    # lm model: include F-stat by default
    # glm also inherits from lm
    if (isTRUE(class(model)[1] == "lm") &&
        inherits(gof, "data.frame") &&
        "statistic" %in% colnames(gof)) {
        gof$F <- gof$statistic
    }

    # vcov_type: nothing if unnamed matrix, vector, or function
    if (is.character(vcov_type) && !vcov_type %in% c("matrix", "vector", "function")) {
        gof$vcov.type <- vcov_type
    }

    # internal customization by modelsummary
    gof_custom <- glance_custom_internal(model)
    if (!is.null(gof_custom) && is.data.frame(gof)) {
        for (n in colnames(gof_custom)) {
            # modelsummary's vcov argument has precedence
            # mainly useful to avoid collision with `fixet::glance_custom`
            if (is.null(vcov_type) || n != "vcov.type") {
                gof[[n]] <- gof_custom[[n]]
            }
        }
    }

    # glance_custom (vcov_type arg is needed for glance_custom.fixest)
    gof_custom <- glance_custom(model)
    if (!is.null(gof_custom) && is.data.frame(gof)) {
        for (n in colnames(gof_custom)) {
            # modelsummary's vcov argument has precedence
            # mainly useful to avoid collision with `fixet::glance_custom`
            if (is.null(vcov_type) || n != "vcov.type") {
                gof[[n]] <- gof_custom[[n]]
            }
        }
    }

    if (inherits(gof, "data.frame")) {
        return(gof)
    }

    warning(sprintf(
'`modelsummary could not extract goodness-of-fit statistics from a model
of class "%s". The package tried a sequence of 2 helper functions:

broom::glance(model)
performance::model_performance(model)

One of these functions must return a one-row `data.frame`. The `modelsummary` website explains how to summarize unsupported models or add support for new models yourself:

https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html',
class(model)[1]))
}


#' Extract goodness-of-fit statistics from a single model using the
#' `broom` package or another package with package which supplies a
#' method for the `generics::glance` generic.
#'
#' @keywords internal
get_gof_broom <- function(model, ...) {

  out <- suppressWarnings(try(
    broom::glance(model, ...),
    silent = TRUE))

  if (!inherits(out, "data.frame")) {
    return("`broom::glance(model)` did not return a data.frame.")
  }

  if (nrow(out) > 1) {
    return("`broom::glance(model)` returned a data.frame with more than 1 row.")
  }

  return(out)
}


#' Extract goodness-of-fit statistics from a single model using
#' the `performance` package
#'
#' @keywords internal
get_gof_parameters <- function(model, ...) {

  # select appropriate metrics to compute
  if ("metrics" %in% names(list(...))) {
    out <- suppressWarnings(try(
      performance::model_performance(model, ...)))
  } else {
    # stan models: r2_adjusted is veeeery slow
    if (inherits(model, "stanreg") ||
        inherits(model, "brmsfit") ||
        inherits(model, "stanmvreg")) {
      # this is the list of "common" metrics in `performance`
      # documentation, but their code includes R2_adj, which produces
      # a two-row glance and gives us issues.
      metrics <- c("LOOIC", "WAIC", "R2", "RMSE")
    } else {
      metrics <- "all"
    }
    out <- suppressWarnings(try(
      performance::model_performance(model, metrics = metrics, ...)))
  }

  # sanity
  if (!inherits(out, "data.frame")) {
    return("`performance::model_performance(model)` did not return a data.frame.")
  }

  if (nrow(out) > 1) {
    return("`performance::model_performance(model)` returned a data.frame with more than 1 row.")
  }

  # cleanup
  out <- insight::standardize_names(out, style = "broom")

  # nobs
  mi <- insight::model_info(model)
  if ("n_obs" %in% names(mi)) {
    out$nobs <- mi$n_obs
  }

  return(out)
}
