#' assert if dependency is installed
#'
#' @keywords internal
assert_dependency <- function(library_name, msg=NULL) {
  if (is.null(msg)) {
    msg <- "Please install the %s package."
  }
  if (!requireNamespace(library_name, quietly = TRUE)) {
    stop(sprintf(msg, library_name))
  }
}

#' check if dependency is installed
#'
#' @keywords internal
check_dependency <- function(library_name) {
  requireNamespace(library_name, quietly = TRUE)
}

#' sanity check
#'
#' @keywords internal
sanity_model_names <- function(modelnames) {
  if (any(modelnames == '')) {
    stop('Model names cannot include empty strings. Please make sure that every object in the `models` list has a unique, non-empty name. If the `models` list has no names at all (NULL), `modelsummary` will create some automatically.')
  }
}

#' sanity check
#'
#' @keywords internal
sanity_align <- function(align, tab) {
  checkmate::assert(
    checkmate::check_character(align, len=1, null.ok = TRUE),
    checkmate::check_character(align, len=ncol(tab), null.ok = TRUE),
    combine="or"
  )
}

#' sanity check
#'
#' @keywords internal
sanity_estimate <- function(estimate) checkmate::assert_character(estimate)

#' sanity_check
#'
#' @keywords internal
sanity_statistic <- function(statistic) {
  checkmate::assert_character(statistic, len=1, null.ok=TRUE)
}

#' sanity check
#'
#' @keywords internal
sanity_title <- function(title) checkmate::assert_character(title, len = 1, null.ok = TRUE)

#' sanity check
#'
#' @keywords internal
sanity_coef <- function(coef_map, coef_rename, coef_omit) {
  checkmate::assert_character(coef_map, null.ok=TRUE)
  checkmate::assert_character(names(coef_map), null.ok=TRUE, unique=TRUE)
  checkmate::assert_character(coef_rename, null.ok=TRUE)
  checkmate::assert_character(names(coef_rename), null.ok=TRUE, unique=TRUE)
  checkmate::assert_string(coef_omit, null.ok = TRUE)
  if (!is.null(coef_rename) & !is.null(coef_map)) {
    stop("coef_map and coef_rename cannot be used together.")
  }
}


#' sanity check
#'
#' @keywords internal
sanity_gof <- function(gof_map, gof_omit) {
  checkmate::assert_string(gof_omit, null.ok = TRUE)
  checkmate::assert_data_frame(gof_map, null.ok = TRUE)
}


#' sanity check
#'
#' @keywords internal
sanity_fmt <- function(fmt) {
  checkmate::assert(
    checkmate::check_character(fmt, len=1),
    checkmate::check_numeric(fmt, len=1, lower=0),
    checkmate::check_function(fmt)
  )
}

#' sanity check
#'
#' @keywords internal
sanity_conf_level <- function(conf_level) {
  checkmate::assert_number(conf_level, lower = 0, upper = .999999999999, null.ok=TRUE)
}

#' sanity check
#'
#' @keywords internal
sanity_factory <- function(factory_dict) {
  check_option <- function(output_type, valid) {
    if (!factory_dict[[output_type]] %in% valid) {
      msg <- paste0("`modelsummary` cannot write a table of type '",
        output_type,
        "' using the ",
        factory_dict[[output_type]],
        " package. You must use one of the following packages: ",
        paste(valid, collapse = ', '),
        ". Consider setting a global option such as: option(modelsummary_",
        output_type,
        "='",
        valid[1],
        "')"
      )
      stop(msg)
    }
  }
  check_option('default', c('gt', 'kableExtra', 'flextable', 'huxtable'))
  check_option('html', c('gt', 'kableExtra', 'flextable', 'huxtable'))
  check_option('rtf', c('gt', 'huxtable'))
  check_option('latex', c('gt', 'kableExtra', 'huxtable'))
  check_option('markdown', c('kableExtra'))
  check_option('word', c('flextable', 'huxtable'))
  check_option('powerpoint', c('flextable', 'huxtable'))
  check_option('png', c('gt', 'flextable', 'kableExtra'))
  check_option('jpg', c('flextable', 'kableExtra'))
}

#' sanity check
#'
#' @keywords internal
sanity_stars <- function(stars) {
  checkmate::assert(
    checkmate::check_flag(stars),
    checkmate::check_numeric(stars, lower = 0, upper = 1, names = 'unique')
  )
}

#' sanity check
#'
#' @keywords internal
sanity_notes <- function(notes) {
  checkmate::assert(
    checkmate::check_list(notes, null.ok = TRUE),
    checkmate::check_character(notes)
  )
  if ('list' %in% class(notes)) {
    for (note in notes) {
      checkmate::assert(
        checkmate::check_character(note),
        checkmate::check_class(note, 'from_markdown')
      )
    }
  }
}

#' sanity check
#'
#' @keywords internal
sanity_output <- function(output) {

  object_types <- c('default', 'gt', 'kableExtra', 'flextable', 'huxtable', 'html', 'latex', 'markdown', 'dataframe', 'data.frame')
  extension_types <- c('html', 'tex', 'md', 'txt', 'docx', 'pptx', 'rtf', 'jpg', 'png')

  checkmate::assert_string(output)

  cond1 <- output %in% object_types
  if (isFALSE(cond1)) {
    extension <- tools::file_ext(output)
    cond2 <- extension %in% extension_types
    if (isTRUE(cond2)) {
      checkmate::assert_path_for_output(output, overwrite = TRUE)
    } else {
      msg <- paste0('The `output` argument must be ',
        paste(object_types, collapse = ', '),
        ', or a valid file path with one of these extensions: ',
        paste(extension_types, collapse = ', '))
      stop(msg)
    }
  }
}


#' sanity check
#'
#' @keywords internal
sanity_add_rows <- function(add_rows, models) {
  if (inherits(add_rows, 'list')) {
    for (i in seq_along(add_rows)) {
      checkmate::assert_character(add_rows[[i]], null.ok = FALSE, len = (length(models) + 1))
    }
  } else if (inherits(add_rows, 'data.frame')) {
    checkmate::assert_true(all(c('section', 'position') %in% colnames(add_rows)))
    checkmate::assert_true(all(colnames(add_rows) %in% c('term', 'section', 'position', names(models))))
  }
}


#' sanity check
#'
#' @keywords internal
sanity_estimate_override <- function(models, estimate_override) {

  checkmate::assert(
    checkmate::check_list(estimate_override, null.ok = TRUE),
    checkmate::check_function(estimate_override, null.ok = TRUE),
    checkmate::check_matrix(estimate_override, null.ok = TRUE),
    checkmate::check_atomic_vector(estimate_override),
    combine="or"
  )

  if (class(estimate_override)[1] == "list") { # must be a simple list
    checkmate::assert_true(length(estimate_override) == length(models))
    for (s in estimate_override) {
      checkmate::assert(
        checkmate::check_function(s),
        checkmate::check_matrix(s),
        checkmate::check_vector(s),
        combine="or"
      )
    }
  } 
}

#' sanity check
#'
#' @keywords internal
sanity_gof <- function(gof_output, gof_custom) {
  checkmate::assert_data_frame(gof_output, nrows = 1, null.ok = FALSE)
  checkmate::assert_data_frame(gof_custom, nrows = 1, null.ok = TRUE)
}

#' sanity check
#'
#' @keywords internal
sanity_tidy <- function(tidy_output, tidy_custom, estimate, statistic, modelclass) {

  # tidy(model)
  checkmate::assert_data_frame(tidy_output, min.rows = 1, min.cols = 3)
  checkmate::assert_true('term' %in% colnames(tidy_output))

  # tidy_custom(model)
  checkmate::assert_data_frame(tidy_custom, nrows = nrow(tidy_output),
    min.cols = 2, null.ok = TRUE)

  if (!is.null(tidy_custom)) {
    checkmate::assert_true('term' %in% colnames(tidy_custom))
    checkmate::assert_true(all(tidy_output$term == tidy_custom$term))
  }

  # columns
  available <- c(colnames(tidy_output), colnames(tidy_custom))

  if (!estimate %in% available) {
    msg <- paste0('For models of class ',
      modelclass,
      ' the `estimate` argument of the `modelsummary()` function must be one of: ',
      paste(available, collapse = ', '))
    stop(msg)
  }

  statistic[statistic == 'conf.int'] <- 'conf.low'
  if (!all(statistic %in% available)) {
    if ('conf.low' %in% available) {
      available <- base::setdiff(available, c('conf.low', 'conf.high'))
      available <- c('conf.int', available)
    }
    msg <- paste0('For models of class ',
      modelclass,
      ' the `statistic` argument of the `modelsummary()` function must be one of: ',
      paste(available, collapse = ', '),
      ' (or possibly conf.int)')
    stop(msg)
  }
}

#' sanity check: datasummary
#'
# warn if a character or logical variable is nested (common mistake)
#' @keywords internal
sanity_ds_nesting_factor <- function(formula, data) {
  idx <- sapply(data, function(x) is.character(x) | is.logical(x))
  idx <- names(idx)[idx]
  idx <- c(paste0('^', idx, ':'), paste0(':', idx, '$'))
  termlabs <- labels(stats::terms(formula))
  warn <- any(sapply(idx, function(x) any(grepl(x, termlabs))))
  if (warn) {
    warning(
      'You are trying to create a nested table by applying the * operator to a character or a logical variable. It is usually a good idea to convert such variables to a factor before calling datasummary: dat$y<-as.factor(dat$y). Alternatively, you could wrap your categorical variable inside Factor() in the datasummary call itself: datasummary(x ~ Factor(y) * z, data)\n')
  }
}

#' sanity check: datasummary_balance
#'
#' @param formula
#' right-handed formulae only
sanity_ds_right_handed_formula <- function(formula) {
  termlabels <- labels(stats::terms(formula))
  if (length(termlabels) > 1) {
    stop("The 'datasummary_table' function only accepts a single right-hand side variable of type factor, character, or logical. If you do not want to transform your variable in the original data, you can wrap it in a Factor() call: datasummary_balance(~Factor(x), data). the name of your variablePlease visit the `modelsummary` website to learn how to build your own, more complex, Table 1. It's easy, I promise! https://vincentarelbundock.github.io/modelsummary/datasummary.html")
  }
}
