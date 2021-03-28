# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when#comment20826625_12429344
# 2012 hadley says "globalVariables is a hideous hack and I will never use it"
# 2014 hadley updates his own answer with globalVariables as one of "two solutions"
globalVariables(c('.', 'term', 'part', 'estimate', 'conf.high', 'conf.low', 'value', 'p.value', 'std.error', 'statistic', 'stars_note', 'logLik', 'formatBicLL', 'section', 'position', 'where', 'ticks', 'statistic1', 'model', 'tmp_grp', 'condition_variable', 'conf_int', 'conf_level'))



#' Model Summary Tables
#'
#' The content of the tables can be altered with the function's arguments, or by
#' calling `options`, as described in the _Details_ section below. The look of
#' the tables can be customized by specifying the `output` argument, and by
#' using functions from one of the supported table customization packages:
#' `kableExtra`, `gt`, `flextable`, `huxtable`.
#' 
#' @param models a model or (optionally named) list of models
#' @param output filename or object type (character string)
#' * Supported filename extensions: .html, .tex, .md, .txt, .png, .jpg.
#' * Supported object types: "default", "html", "markdown", "latex", "latex_tabular", "data.frame", "gt", "kableExtra", "huxtable", "flextable".
#' * To change the default output format, type `options(modelsummary_default = "latex")`, where `latex` can be any of the valid object types listed above. 
#' * Warning: the `output` argument \emph{cannot} be used when customizing tables with external packages. See the 'Details' section below.
#' @param fmt determines how to format numeric values
#' * integer: the number of digits to keep after the period `format(round(x, fmt), nsmall=fmt)`
#' * character: passed to the `sprintf` function (e.g., '\%.3f' keeps 3 digits with trailing zero). See `?sprintf`
#' * function: returns a formatted character string.
#' @param stars to indicate statistical significance
#' * FALSE (default): no significance stars.
#' * TRUE: *=.1, **=.05, ***=.01
#' * Named numeric vector for custom stars such as `c('*' = .1, '+' = .05)`
#' @param statistic vector of strings or `glue` strings which select uncertainty
#' statistics to report vertically below the estimate. NULL omits all
#' uncertainty statistics.
#' * "conf.int", "std.error", "statistic", "p.value", "conf.low", "conf.high",
#'    or any column name produced by: `get_estimates(model)`
#' * `glue` package strings with braces, such as: 
#'   - `"{p.value} [{conf.low}, {conf.high}]"`
#'   - `"Std.Error: {std.error}"`
#' * Note: Parentheses are added automatically unless the string includes `glue` curly braces `{}`.
#' * Note: To report uncertainty statistics \emph{next} to coefficients, you can #'   supply a `glue` string to the `estimate` argument.
#' @param vcov robust standard errors and other manual statistics. The `vcov`
#'   argument accepts five types of input (see the 'Details' and 'Examples'
#'   sections below):
#' * string, vector, or list of strings: "robust", "HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "stata", or "classical" (alias "constant" or "iid").
#' * formula or list of formulas with the cluster variable(s) on the right-hand side (e.g., ~clusterid).
#' * function or list of functions which return variance-covariance matrices with row and column names equal to the names of your coefficient estimates (e.g., `stats::vcov`, `sandwich::vcovHC`).
#' * list of `length(models)` variance-covariance matrices with row and column names equal to the names of your coefficient estimates.
#' * a list of length(models) vectors with names equal to the names of your coefficient estimates. See 'Examples' section below. 
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_map character vector. Subset, rename, and reorder coefficients.
#' Coefficients omitted from this vector are omitted from the table. The order
#' of the vector determines the order of the table.  `coef_map` can be a named
#' or an unnamed character vector (see the Examples section below). If
#' `coef_map` is a named vector, its values define the labels that must appear
#' in the table, and its names identify the original term names stored in the
#' model object: `c("hp:mpg"="HPxM/G")`.
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table using `grepl(perl=TRUE)`.
#' @param coef_rename named character vector. Values refer to the variable names
#' that will appear in the table. Names refer to the original term names stored
#' in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term.
#' @param gof_map 
#' * NULL (default): the `modelsummary::gof_map` dictionary is used for formatting, and all unknown statistic are included.
#' * data.frame with 3 columns named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples' section below.
#' * list of lists, each of which includes 3 elements named "raw", "clean", "fmt". Unknown statistics are omitted. See the 'Examples section below'.
#' @param gof_omit string regular expression. Omits all matching gof statistics from
#' the table (using `grepl(perl=TRUE)`).
#' @param group string group identifier for models that produce groups of related
#' parameters (e.g., multinomial logit with outcome levels, or components of a
#' GAMLSS model). Valid values are column names in the data.frame produced by
#' `get_estimates(model)`. To display groups side-by-side, consider the
#' `modelsummary_wide` function.
#' @param add_rows a data.frame (or tibble) with the same number of columns as
#' your main table. By default, rows are appended to the bottom of the table.
#' You can define a "position" attribute of integers to set the row positions.
#' See Examples section below.
#' @param title string
#' @param notes list or vector of notes to append to the bottom of the table.
#' @param estimate string or `glue` string of the estimate to display (or a
#' vector with one string per model). Valid entries include any column name of
#' the data.frame produced by `get_estimates(model)`. Examples:
#' * `"estimate"`
#' * `"{estimate} ({std.error}){stars}"`
#' * `"{estimate} [{conf.low}, {conf.high}]"`
#' @param align A character string of length equal to the number of columns in
#' the table.  "lcr" means that the first column will be left-aligned, the 2nd
#' column center-aligned, and the 3rd column right-aligned.
#' @param ... all other arguments are passed through to the extractor and
#' table-making functions. This allows users to pass arguments directly to
#' `modelsummary` in order to affect the behavior of other functions behind
#' the scenes. Examples include:
#' * `broom::tidy(exponentiate=TRUE)` to exponentiate logistic regression
#' * `kableExtra::kbl(escape=FALSE)` to avoid escaping math characters in #'   `kableExtra` tables.
#' * `performance::model_performance(metrics="RMSE")` to select goodness-of-fit #'   statistics to extract using the `performance` package (must have set #'   `options(modelsummary_get="easystats")` first).
#' @return a regression table in a format determined by the `output` argument.
#' @importFrom generics glance tidy
#' @details 
#'
#' `options`
#' 
#' `modelsummary` supports 4 table-making packages: `kableExtra`, `gt`,
#' `flextable`, and `huxtable`. Some of these packages have overlapping
#' functionalities. For example, 3 of those packages can export to LaTeX. To
#' change the default backend used for a specific file format, you can use
#' the `options` function:
#'
#' options(modelsummary_html = 'kableExtra')
#' options(modelsummary_latex = 'gt')
#' options(modelsummary_word = 'huxtable')
#' options(modelsummary_png = 'gt')
#'
#' `modelsummary` can use two sets of packages to extract information from
#' statistical models: `broom` and the `easystats` family (`performance` and
#' `parameters`). By default, it uses `broom` first and `easystats` as a
#' fallback if `broom` fails. You can change the order of priorities 
#' or include goodness-of-fit extracted by *both* packages by setting:
#'
#' `options(modelsummary_get = "easystats")`
#' `options(modelsummary_get = "easystats")`
#'
#'
#' `output` argument:
#'
#' When a file name is supplied to the `output` argument, the table is written
#' immediately to file. If you want to customize your table by post-processing
#' it with an external package, you need to choose a different output format
#' and saving mechanism. Unfortunately, the approach differs from package to
#' package:
#' * `gt`: set `output="gt"`, post-process your table, and use the `gt::gtsave` function.
#' * `kableExtra`: set `output` to your destination format (e.g., "latex", "html", "markdown"), post-process your table, and use `kableExtra::save_kable` function.
#'
#'
#' `vcov` argument:
#'
#' To use a string such as "robust" or "HC0", your model must be supported
#' by the `sandwich` package. This includes objects such as: lm, glm,
#' survreg, coxph, mlogit, polr, hurdle, zeroinfl, and more.
#' 
#' "classical", "iid", and "constant" are aliases which do not modify
#' uncertainty estimates and simply report the default standard errors stored
#' in the model object.
#'
#' One-sided formulas such as `~clusterid` are passed to the `sandwich::vcovCL`
#' function.
#' 
#' Matrices and functions producing variance-covariance matrices are first
#' passed to `lmtest`. If this does not work, `modelsummary` attempts to take
#' the square root of the diagonal to adjust "std.error", but the other
#' uncertainty estimates are not be adjusted.
#'
#' Numeric vectors are formatted according to `fmt` and placed in brackets.
#' Character vectors printed as given, without parentheses. 
#'
#' If your model type is supported by the `lmtest` package, the
#' `vcov` argument will try to use that package to adjust all the
#' uncertainty estimates, including "std.error", "statistic", "p.value", and
#' "conf.int". If your model is not supported by `lmtest`, only the "std.error"
#' will be adjusted by, for example, taking the square root of the matrix's
#' diagonal.
#' @examples
#' \dontrun{
#'
#' # The `modelsummary` website includes \emph{many} examples and tutorials:
#' # https://vincentarelbundock.github.io/modelsummary
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
#' # statistic
#' modelsummary(models, statistic = NULL)
#' modelsummary(models, statistic = 'p.value')
#' modelsummary(models, statistic = 'statistic')
#' modelsummary(models, statistic = 'conf.int', conf_level = 0.99)
#' modelsummary(models, statistic = c("t = {statistic}",
#'                                    "se = {std.error}",
#'                                    "conf.int"))
#' 
#' # estimate
#' modelsummary(models, 
#'   statistic = NULL,
#'   estimate = "{estimate} [{conf.low}, {conf.high}]")
#' modelsummary(models,
#'   estimate = c("{estimate}{stars}",
#'                "{estimate} ({std.error})"))
#' 
#' # vcov
#' modelsummary(models, vcov = "robust")
#' modelsummary(models, vcov = list("classical", "stata"))
#' modelsummary(models, vcov = sandwich::vcovHC)
#' modelsummary(models, 
#'   vcov = list(stats::vcov, sandwich::vcovHC))
#' modelsummary(models, 
#'   vcov = list(c("(Intercept)"="", "Height"="!"),
#'                             c("(Intercept)"="", "Height"="!", "Volume"="!!")))
#'
#' # coef_rename
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#'
#' # coef_map
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#' modelsummary(models, coef_map = c('Volume', 'Height'))
#'
#' # title
#' modelsummary(models, title = 'This is the title')
#'
#' # add_rows
#' rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
#'   'Empty row', '-', '-',
#'   'Another empty row', '?', '?')
#' attr(rows, 'position') <- c(1, 3)
#' modelsummary(models, add_rows = rows)
#'
#' # notes
#' modelsummary(models, notes = list('A first note', 'A second note'))
#'
#' # gof_map: data.frame
#' gm <- modelsummary::gof_map
#' gof_custom$omit[gof_custom$raw == 'deviance'] <- FALSE
#' gof_custom$fmt[gof_custom$raw == 'r.squared'] <- "%.5f"
#' modelsummary(models, gof_map = gof_custom)
#' 
#' # gof_map: list of lists
#' f1 <- function(x) format(round(x, 3), big.mark=",")
#' f2 <- function(x) format(round(x, 0), big.mark=",")
#' gm <- list(
#'   list("raw" = "nobs", "clean" = "N", "fmt" = f2),
#'   list("raw" = "AIC", "clean" = "aic", "fmt" = f1))
#' modelsummary(models,
#'   fmt = f1,
#'   gof_map = gm)
#' 
#' }
#'
#' @export
modelsummary <- function(
  models,
  output      = "default",
  fmt         = 3,
  estimate    = "estimate",
  statistic   = "std.error",
  vcov        = NULL,
  conf_level  = 0.95,
  stars       = FALSE,
  coef_map    = NULL,
  coef_omit   = NULL,
  coef_rename = NULL,
  gof_map     = NULL,
  gof_omit    = NULL,
  group       = NULL,
  add_rows    = NULL,
  align       = NULL,
  notes       = NULL,
  title       = NULL,
  ...) {


  # sanitation
  sanity_ellipsis(vcov, ...)        # before sanitize_vcov
  models <- sanitize_models(models) # before sanitize_vcov
  vcov <- sanitize_vcov(vcov, length(models), ...)
  number_of_models <- max(length(models), length(vcov))
  estimate <- sanitize_estimate(estimate, number_of_models)
  sanity_output(output)
  sanity_statistic(statistic)
  sanity_conf_level(conf_level)
  sanity_coef(coef_map, coef_rename, coef_omit)
  sanity_gof_map(gof_map, gof_omit)
  sanity_stars(stars)
  sanity_fmt(fmt)


  # output
  output_format <- parse_output_arg(output)$output_format


  # model names dictionary: use unique names for manipulation
  if (is.null(names(models))) {
    model_names <- paste("Model", 1:number_of_models)
  } else {
    model_names <- names(models)
  }
  model_id <- paste("Model", 1:number_of_models)


  # estimates: extract and combine
  est <- list()

  for (i in 1:number_of_models) {

    # recycling when 1 model and many vcov
    j <- ifelse(length(models) == 1, 1, i)

    # extract estimates using broom or parameters
    if (!any(grepl("conf", c(estimate, statistic)))) {
      conf_level <- NULL
    }

    tmp <- get_estimates(models[[j]], conf_level = conf_level, ...)

    tmp <- format_estimates(
      est        = tmp,
      model      = models[[j]],
      fmt        = fmt,
      estimate   = estimate[[i]],
      statistic  = statistic,
      vcov       = vcov[[i]],
      conf_level = conf_level,
      stars      = stars,
      group      = group,
      ...
    )

    # rename and subset before merging to collapse rows
    tmp <- map_omit_rename_estimates(
      tmp,
      coef_rename = coef_rename,
      coef_map = coef_map,
      coef_omit = coef_omit)

    # model name is temporarily a unique id
    colnames(tmp)[4] <- model_id[i]

    # assign to estimate list
    est[[model_id[i]]] <- tmp

  }

  # order: store for later
  if (is.null(coef_map)) {
    term_order <- lapply(est, function(x) x$term)
    term_order <- unique(unlist(term_order))
  } else {
    term_order <- unique(coef_map)
  }

  # merge models
  f <- function(x, y) {
    merge(x, y, all = TRUE, sort = FALSE, by = c("group", "term", "statistic"))
  }
  est <- Reduce(f, est) 

  # re-order columns
  est$part <- "estimates"
  est <- est[, unique(c("part", "group", "term", "statistic", names(est)))]

  # re-order rows
  idx <- match(est[["term"]], term_order)
  est <- est[order(est$part, est$group, idx, est[["statistic"]]),]

  # vcov_type: at least two distinct strings or formulas
  vcov_type <- get_vcov_type(vcov) 

  # gof: extract and combine
  gof <- list()

  for (i in 1:number_of_models) {

    # recycling models when multiple vcov
    j <- ifelse(length(models) == 1, 1, i)
      
    gof[[i]] <- get_gof(models[[j]], ...)
    gof[[i]] <- format_gof(models[[j]],
                           gof[[i]],
                           fmt = fmt,
                           gof_map = gof_map,
                           vcov_type = vcov_type[[i]],
                           ...)
    colnames(gof[[i]])[2] <- model_id[i]
  }

  # merge
  f <- function(x, y) merge(x, y, all=TRUE, sort=FALSE, by="term")
  gof <- Reduce(f, gof)

  # subset, rename, reorder
  gof <- map_omit_gof(gof, gof_omit, gof_map)

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

    if (is.null(group)) {
        tab$group <-NULL
    } else {
      for (i in nrow(tab):2) {
        if (tab$part[i] == "estimates" &&
            tab$group[i - 1] == tab$group[i]) {
            tab$group[i] <- ""
        }
      }
    }
              

    # duplicate term labels
    idx <- grepl("modelsummary_tmp\\d+$", tab$statistic) &
           tab$statistic != "modelsummary_tmp1"
    tab$term[idx] <- ""

    # clean table but keep metadata for data.frame output
    if (output_format != "dataframe") {
      tab$statistic <- tab$part <- NULL

      # HACK: arbitrary spaces to avoid name conflict
      colnames(tab)[colnames(tab)=="term"] <- "       "
      colnames(tab)[colnames(tab)=="group"] <- "        "
    }

  }


  # not optimal, but messes up all the tests, and do we need a "group" column?
  # need to fix table alignment if this isn't what we want
  tab$group <- NULL

  # align
  if (is.null(align) && is.null(group)) {
    align <- paste0("l", strrep("c", ncol(tab) - 1))
  }


  # restore original model names
  idx <- match(model_id, colnames(tab))
  colnames(tab)[idx] <- model_names
    

  # build table
  factory(
    tab,
    align    = align,
    fmt      = fmt,
    hrule    = hrule,
    notes    = notes,
    output   = output,
    title    = title,
    add_rows = add_rows,
    ...
  )

}

#' `msummary()` is a shortcut to `modelsummary()`
#'
#' @inherit modelsummary
#' @keywords internal
#' @export
msummary <- modelsummary



#' rename and reorder estimates
#'
#' @keywords internal
map_omit_rename_estimates <- function(estimates,
                             coef_rename,
                             coef_map,
                             coef_omit) {
                                 
    # coef_rename
    if (!is.null(coef_rename)) {
        if (is.character(coef_rename)) {
            dict <- coef_rename
        } else if (is.function(coef_rename)) {
            dict <- stats::setNames(coef_rename(estimates$term), estimates$term)
        }
        estimates$term <- replace_dict(estimates$term, dict)
    }

    # coef_map
    if (!is.null(coef_map)) {
        if (is.null(names(coef_map))) {
            coef_map <- stats::setNames(coef_map, coef_map)
        }
        estimates <- estimates[estimates$term %in% names(coef_map), , drop=FALSE]
        estimates$term <- replace_dict(estimates$term, coef_map)
    }

    # coef_omit
    if (!is.null(coef_omit)) {
        idx <- !grepl(coef_omit, estimates$term, perl=TRUE)
        estimates <- estimates[idx, , drop=FALSE]
    }

    # make sure no duplicate estimate names *within* a single model. this
    # cannot be in input sanity checks. idx paste allows multiple statistics.
    idx <- paste(estimates$term, estimates$statistic)
    if (anyDuplicated(idx) > 2) {
      stop('Two coefficients from a single model cannot share the same name.')
    }

  return(estimates)
}


#' Internal function to subset, rename and re-order gof statistics
#'
#' @keywords internal
map_omit_gof <- function(gof, gof_omit, gof_map) {

  if (nrow(gof) == 0) {
    return(gof)
  }

  # row identifier
  gof$part <- "gof"
  gof <- gof[, unique(c("part", "term", names(gof)))]

  # omit
  if (!is.null(gof_omit)) {
    idx <- !grepl(gof_omit, gof$term, perl=TRUE)
    gof <- gof[idx, , drop=FALSE]
  }

  # map
  if (is.null(gof_map)) {
    # assign here and not in the function definition because we use NULL to
    # figure out if F-stat should be included by default for lm models.
    gm_list <- get("gof_map", envir = loadNamespace("modelsummary"))
    gm_list <- lapply(1:nrow(gm_list), function(i) gm_list[i, ])
  } else if (inherits(gof_map, "data.frame")) {
    gm_list <- lapply(1:nrow(gof_map), function(i) gof_map[i, ])
  } else {
    gm_list <- gof_map
  }

  gm_raw <- sapply(gm_list, function(x) x$raw)
  gm_clean <- sapply(gm_list, function(x) x$clean)
  gof_names <- gm_clean[match(gof$term, gm_raw)]
  gof_names[is.na(gof_names)] <- gof$term[is.na(gof_names)]
  gof$term <- gof_names
  idx <- match(gof$term, gm_clean)
  gof <- gof[order(idx, gof$term), ]

  return(gof)
}
