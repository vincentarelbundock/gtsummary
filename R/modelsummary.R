# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'part', 'estimate', 'conf.high', 'conf.low', 'value', 'p.value', 'std.error', 'statistic', 'stars_note', 'logLik', 'formatBicLL', 'section', 'position', 'where', 'ticks', 'statistic1', 'model', 'tmp_grp', 'condition_variable'))


#' Deprecated function
#'
#' @param ... any argument
#' @export
extract_models <- function(...) {
  stop('This function is deprecated. Consider using `modelsummary(output="data.frame")` instead.')
}

#' Beautiful, customizable summaries of statistical models
#'
#' @param models a single model object or a (potentially named) list of models
#' to summarize
#' @param output filename or object type (string)
#' \itemize{
#'   \item Supported filename extensions: .html, .tex, .md, .txt, .png, .jpg.
#'   \item Supported object types: "default", "html", "markdown", "latex", "data.frame", "gt", "kableExtra", "huxtable", "flextable".
#'   \item When a file name is supplied to the `output` argument, the table is written immediately to file. If you want to customize your table by post-processing it with functions provided by the `gt` or `kableExtra` packages, you need to choose a different output format (e.g., "gt", "latex", "html", "markdown"), and you need to save the table after post-processing using the `gt::gtsave`, `kableExtra::save_kable`, or `cat` functions.
#' }
#' @param fmt an integer, string, or function which determines how to format numeric values:
#' \itemize{
#'   \item integer: the number of digits to keep after the period (`format(round(x, fmt), nsmall=fmt)`)
#'   \item character: string is passed to the `sprintf` function. '\%.3f' will keep 3 digits after the decimal point with trailing zero. '\%.5f' will keep 5 digits. '\%.3e' will use exponential notation. See `?sprintf` for more options.
#'   \item function: a function which returns a formatted character string
#' }
#' @param stars to indicate statistical significance
#' \itemize{
#'   \item FALSE (default): no significance stars.
#'   \item TRUE: *=.1, **=.05, ***=.01
#'   \item Named numeric vector for custom stars such as `c('*' = .1, '+' = .05)`
#' }
#' @param statistic character vector of raw strings or glue strings (see examples) which represent the uncertainty statistics to report vertically. Parentheses are added automatically unless the value includes `glue` curly braces {}. Acceptable values:
#' \itemize{
#'   \item Typical values: "conf.int", "std.error", "statistic", "p.value", "conf.low", "conf.high".
#'   \item Alternative values: any column name produced by `broom::tidy(model)`
#'   \item `glue` package strings with braces such as: 
#'   \item "\{estimate\} [\{conf.low\}, \{conf.high\}]"
#' }
#' @param statistic_override manually override statistics. Accepts three types of input:
#' \itemize{
#'   \item a function or list of functions of length(models) which produce variance-covariance matrices with row and column names equal to the names of your coefficient estimates. For example, `R` supplies the `vcov` function, and the `sandwich` package supplies `vcovHC`, `vcovHAC`, etc. If the `lmtest` package is installed, `modelsummary` will try to use it to override: "std.error", "statistic", "p.value", "conf.int". If the `lmtest` package is not installed, `modelsummary` will override "std.error" by using the square root of the vcov matrix diagonal.
#'   \item a list of length(models) variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
#'   \item a list of length(models) vectors with names equal to the names of your coefficient estimates. Numeric vectors are formatted according to `fmt` and placed in brackets, character vectors printed as given.
#' }
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_map named character vector. Values refer to the variable names
#' that will appear in the table. Names refer to the original term names stored
#' in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' Coefficients that are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table (using `grepl(perl=TRUE)`).
#' @param coef_rename named character vector. Values refer to the variable names
#' that will appear in the table. Names refer to the original term names stored
#' in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' @param gof_map data.frame with four columns: `raw`, `clean`, `fmt`, and
#' `omit`. If `gof_map` is NULL, then `modelsummary` will use this data frame
#' by default: `modelsummary::gof_map` By default, all the statistics produced
#' by `broom::glance` will be included unless they are omitted explicitly in
#' `gof_map`.
#' @param gof_omit string regular expression. Omits all matching gof statistics from
#' the table (using `grepl(perl=TRUE)`).
#' @param add_rows a data.frame (or tibble) with the same number of columns as
#' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the row positions.
#' See examples.
#' @param title string
#' @param notes list or vector of notes to append to the bottom of the table.
#' @param estimate character name of the estimate to display. Must be a column
#' name in the data.frame produced by `tidy(model)`. In the vast majority of
#' cases, the default value of this argument should not be changed.
#' @param align A character string of length equal to the number of columns in
#' the table.  "lcr" means that the first column will be left-aligned, the 2nd
#' column center-aligned, and the 3rd column right-aligned.
#' @param ... all other arguments are passed to the `tidy` and `glance` methods
#' used to extract estimates from the model. For example, this allows users to
#' set `exponentiate=TRUE` to exponentiate logistic regression coefficients.
#' @param statistic_vertical deprecated argument. Supply a vector of strings or `glue` strings to the `estimate` instead.
#' @return a regression table in a format determined by the `output` argument.
#' @importFrom generics glance tidy
#' @examples
#' \dontrun{
#'
#' library(modelsummary)
#'
#' # load data and estimate models
#' data(trees)
#' models <- list()
#' models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
#' models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
#'
#' # simple table
#' modelsummary(models)
#'
#' # confidence intervals, p values, or t-stats instead of standard errors
#' modelsummary(models, statistic = 'conf.int', conf_level = 0.99)
#' modelsummary(models, statistic = 'p.value', conf_level = 0.99)
#' modelsummary(models, statistic = 'statistic', conf_level = 0.99)
#'
#' # coef_rename: rename coefficients
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # coef_map: rename, re-order, and omit coefficients
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # titles
#' modelsummary(models, title = 'This is the title')
#'
#' # title with italicized text
#' modelsummary(models, title = gt::md('This is *the* title'))
#'
#' # add_rows: we use `tribble` from the `tibble` package to build a data.frame
#' # more easily. Then, we assign an attribute to determine each row's position.
#' rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
#'   'Empty row', '-', '-',
#'   'Another empty row', '?', '?')
#' attr(rows, 'position') <- c(1, 3)
#' modelsummary(models, add_rows = rows)
#'
#' # notes at the bottom of the table (here, the second note includes markdown bold characters)
#' modelsummary(models, notes = list('A first note', gt::md('A **bold** note')))
#'
#' # modify list of GOF statistics and their format using the built-in
#' # 'gof_map' data frame as a starting point
#' gof_custom <- modelsummary::gof_map
#' gof_custom$omit[gof_custom$raw == 'deviance'] <- FALSE
#' gof_custom$fmt[gof_custom$raw == 'r.squared'] <- "%.5f"
#' modelsummary(models, gof_map = gof_custom)
#' }
#'
#' # see the README on github for a lot more examples:
#' # https://github.com/vincentarelbundock/modelsummary
#' @export
modelsummary <- function(
  models,
  output = "default",
  fmt = 3,
  statistic = "std.error",
  statistic_override = NULL,
  stars = FALSE,
  conf_level = 0.95,
  coef_map = NULL,
  coef_omit = NULL,
  coef_rename = NULL,
  gof_map = NULL,
  gof_omit = NULL,
  estimate = "estimate",
  add_rows = NULL,
  title = NULL,
  notes = NULL,
  align = NULL,
  statistic_vertical = NULL,
  ...) {

  # sanity check functions are hosted in R/sanity_checks.R
  sanity_output(output)
  sanity_statistic(statistic)
  sanity_estimate(estimate)
  sanity_statistic_override(models, statistic_override)
  sanity_conf_level(conf_level)
  sanity_coef(coef_map, coef_rename, coef_omit)
  # sanity_gof(gof_map, gof_omit)
  sanity_stars(stars)
  sanity_fmt(fmt)

  # deprecation: statistic, statistic_override, statistic_vertical
  if (!is.null(statistic_vertical)) {
    warning("The `statistic_vertical` argument is deprecated. To present
            uncertainty estimates next to the main estimate, you can supply a
            `glue` string to the `estimate` argument. See examples.")
  }

  # output
  output_format <- parse_output_arg(output)$output_format

  # extra arguments
  ellipsis <- list(...)

  # models must be a list
  if (!inherits(models, "list")) {
    models <- list(models)
  }

  # model names dictionary: use unique names for manipulation
  if (is.null(names(models))) {
    model_names <- paste("Model", 1:length(models))
  } else {
    model_names <- names(models)
  }
  model_id <- paste("Model", 1:length(models))

  # statistic_override must be a list
  if (!inherits(statistic_override, "list")) {
    statistic_override <- rep(list(statistic_override), length(models))
  }

  # estimates: extract and combine
  est <- list()

  for (i in seq_along(models)) {

    tmp <- extract_estimates(
      model              = models[[i]],
      fmt                = fmt,
      estimate           = estimate,
      statistic          = statistic,
      statistic_override = statistic_override[[i]],
      conf_level         = conf_level,
      stars              = stars,
      ...
    )

    # coef_rename: before merge to collapse rows
    if (!is.null(coef_rename)) {
      tmp$term <- replace_dict(tmp$term, coef_rename)
    }

    # coef_map
    if (!is.null(coef_map)) {
      tmp <- tmp[tmp$term %in% names(coef_map), , drop=FALSE]
      tmp$term <- replace_dict(tmp$term, coef_map)
    }

    # coef_omit
    if (!is.null(coef_omit)) {
      idx <- !grepl(coef_omit, tmp$term, perl=TRUE)
      tmp <- tmp[idx, , drop=FALSE]
    }

    # make sure no duplicate estimate names *within* a single model. this
    # cannot be in input sanity checks. idx paste allows multiple statistics.
    idx <- paste(tmp$term, tmp$statistic)
    if (anyDuplicated(idx) > 2) {
      stop('Two coefficients from a single model cannot share the same name. Check model ', i)
    }

    # model name is temporarily a unique id
    colnames(tmp)[3] <- model_id[i]

    # assign
    est[[model_id[i]]] <- tmp

  }

  term_order <- lapply(est, function(x) x$term)
  term_order <- unique(unlist(term_order))

  f <- function(x, y) merge(x, y, all=TRUE, sort=FALSE, by=c("term", "statistic"))
  est <- Reduce(f, est) 
  est$part <- "estimates"
  est <- est[, unique(c("part", "term", "statistic", names(est)))]

  # default order
  idx <- match(est[["term"]], term_order)
  est <- est[order(idx, est[["statistic"]]),]

  # coef_map
  if (!is.null(coef_map)) {
    coef_map <- intersect(coef_map, term_order)
    est <- est[est[["term"]] %in% coef_map, , drop=FALSE]
    idx <- match(est[["term"]], coef_map)
    est <- est[order(idx, est[["statistic"]]),]
  }

  # gof: extract and combine
  gof <- list()

  for (i in seq_along(models)) {
    gof[[i]] <- extract_gof(models[[i]], fmt=fmt, gof_map=gof_map, ...)
    colnames(gof[[i]])[2] <- model_id[i]
  }

  f <- function(x, y) merge(x, y, all=TRUE, sort=FALSE, by="term")
  gof <- Reduce(f, gof)

  if (nrow(gof) > 0) {

    # gof row identifier
    gof$part <- "gof"
    gof <- gof[, unique(c("part", "term", names(gof)))]

    # gof_omit
    if (!is.null(gof_omit)) {
      idx <- !grepl(gof_omit, gof$term, perl=TRUE)
      gof <- gof[idx, , drop=FALSE]
    }

    # gof_map
    if (nrow(gof) > 0) {
      if (is.null(gof_map)) {
        # assign here and not in the function definition because we use NULL to
        # figure out if F-stat should be included by default for lm models.
        gof_map <- get("gof_map", as.environment("package:modelsummary"))
      }
      gof <- gof[!gof$term %in% gof_map$raw[gof_map$omit], , drop=FALSE]
      gof_names <- gof_map$clean[match(gof$term, gof_map$raw)]
      gof_names[is.na(gof_names)] <- gof$term[is.na(gof_names)]
      gof$term <- gof_names
      idx <- match(gof$term, gof_map$clean)
      gof <- gof[order(idx, gof$term), ]
    }

  } 

  # combine estimates and gof
  if (nrow(gof) > 0) {
    tab <- bind_rows(est, gof)
  } else {
    tab <- est
  }

  # empty cells
  tab[is.na(tab)] <- ''

  # interaction : becomes ×
  if (is.null(coef_map)) {
    if (output_format != 'rtf') {
      idx <- tab$part != 'gof'
      tab$term <- ifelse(idx, gsub(':', ' \u00d7 ', tab$term), tab$term)
    }
  }

  # measure table
  hrule <- match('gof', tab$part)
  if (!is.na(hrule) &&
      !is.null(add_rows) &&
      !is.null(attr(add_rows, 'position'))) {
    hrule <- hrule + sum(attr(add_rows, 'position') < hrule)
  }
  if (is.na(hrule)) {
    hrule <- NULL
  }

  # stars
  if (!isFALSE(stars)) {
    stars_note <- make_stars_note(stars)
    if (is.null(notes)) {
      notes <- stars_note
    } else {
      notes <- c(stars_note, notes)
    }
  }


  # data.frame output keeps redundant info
  if (output_format != "dataframe") {

    # duplicate term labels
    idx <- grepl("modelsummary_tmp\\d+$", tab$statistic) &
           tab$statistic != "modelsummary_tmp1"
    tab$term[idx] <- ""

    # clean table but keep metadata for data.frame output
    if (output_format != "dataframe") {
      tab$statistic <- tab$part <- NULL

      # HACK: arbitrary 7 spaces to avoid name conflict
      colnames(tab)[colnames(tab)=="term"] <- "       "
    }

  }

  # column alignment (after removing extraneous columns)
  if (is.null(align)) {
    align <- paste0("l", strrep("c", ncol(tab) - 1))
  }

  # restore original model names
  idx <- match(model_id, colnames(tab))
  colnames(tab)[idx] <- model_names

  # build table
  factory(
    tab,
    align = align,
    fmt = fmt,
    hrule = hrule,
    notes = notes,
    output = output,
    title = title,
    add_rows = add_rows,
    ...
  )

}

#' Beautiful, customizable summaries of statistical models
#'
#' `msummary()` is a shortcut to `modelsummary()`
#' @inherit modelsummary
#' @export
msummary <- modelsummary
