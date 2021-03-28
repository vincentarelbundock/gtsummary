#' Extract model estimates. A mostly internal function with some potential uses
#' outside.
#'
#' @inheritParams modelsummary
#' @param model a single model object
#' @export
get_estimates <- function(model, conf_level = .95, ...) {

    if (is.null(conf_level)) {
        conf_int = FALSE
    } else {
        conf_int = TRUE
    }

    # priority
    get_priority <- getOption("modelsummary_get", default = "broom")
    checkmate::assert_choice(get_priority, choices = c("broom", "easystats", "parameters", "performance", "all"))
    if (get_priority %in% c("easystats", "parameters", "performance")) {
        funs <- list(get_estimates_broom, get_estimates_parameters)
    } else {
        funs <- list(get_estimates_parameters, get_estimates_broom)
    }

    warning_msg <- NULL
    out <- NULL

    for (f in funs) {
        if (!inherits(out, "data.frame") || nrow(out) == 0) {
            out <- f(model,
                    conf_int = conf_int,
                    conf_level = conf_level,
                    ...)
            if (is.character(out)) {
                warning_msg <- c(warning_msg, out)
            }
        }
    }

    if (inherits(out, "data.frame")) {
        return(out)
    }

    stop(sprintf(
'`modelsummary could not extract the required information from a model
of class "%s". The package tried a sequence of 2 helper functions to extract
estimates:

broom::tidy(model)
parameters::parameters(model)

To draw a table, one of these commands must return a `data.frame` with a
column named "term". The `modelsummary` website explains how to summarize
unsupported models or add support for new models yourself:

https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

These errors messages were generated during extraction:
%s',
    class(model)[1], paste(warning_msg, collapse = "\n")))
}


get_estimates_broom <- function(model, conf_int, conf_level, ...) {

    if (isTRUE(conf_int)) {
        out <- suppressWarnings(try(
            broom::tidy(model, conf.int = conf_int, conf.level = conf_level, ...), 
            silent=TRUE))
    } else {
        out <- suppressWarnings(try(
            broom::tidy(model, conf.int = conf_int, ...), 
            silent=TRUE))
    }

    if (!inherits(out, "data.frame") || nrow(out) < 1) {
        return("`broom::tidy(model)` did not return a valid data.frame.")
    }

    if (!"term" %in% colnames(out)) {
        return("`broom::tidy(model)` did not return a data.frame with a `term` column.")
    }

    return(out)

}


get_estimates_parameters <- function(model, conf_int, conf_level, ...) {

    f <- tidy_easystats <- function(model, ...) {
        msg <- utils::capture.output(out <- parameters::model_parameters(model, ...))
        parameters::standardize_names(out, style="broom")
    }

    if (isTRUE(conf_int)) {
        out <- suppressWarnings(try(
            f(model, ci = conf_level, ...),
            silent = TRUE))
    } else {
        out <- suppressWarnings(try(
            f(model, ...),
            silent = TRUE))
    }

    if (!inherits(out, "data.frame") || nrow(out) < 1) {
        return("`parameters::parameters(model)` did not return a valid data.frame.")
    }

    if (!"term" %in% colnames(out)) {
        return("`parameters::parameters(model)` did not return a data.frame with a `term` column.")
    }

    return(out)
}
