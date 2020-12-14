#' Beautiful, customizable summaries of statistical models
#'
#' `modelsummary_wide` is a specialized function to display groups of
#' parameters from a single model in separate columns. This can be useful, for
#' example, to display the different levels of coefficients in a multinomial
#' regression model (e.g., `nnet::multinom`). The `coef_group` argument
#' specifies the name of the group identifier.
#'
#' @param coef_group the name of the coefficient groups to use as columns (NULL
#' or character). If `coef_group` is NULL, `modelsummary` tries to guess the
#' correct coefficient group identifier. To be valid, this identifier must be a
#' column in the data.frame produced by `tidy(model)`. Note: you may have to
#' load the `broom` or `broom.mixed` package before executing `tidy(model)`.
#' @inheritParams modelsummary
#' @return a regression table in a format determined by the `output` argument.
#' @export
modelsummary_wide <- function(models,
  output = "default",
  fmt = '%.3f',
  statistic = "std.error",
  statistic_override = NULL,
  conf_level = 0.95,
  stars = FALSE,
  coef_group = NULL,
  coef_map = NULL,
  coef_omit = NULL,
  coef_rename = NULL,
  gof_map = NULL,
  gof_omit = NULL,
  add_rows = NULL,
  title = NULL,
  notes = NULL,
  estimate = "estimate",
  statistic_vertical = NULL,
  ...) {

  # models must be a list of models
  if (!'list' %in% class(models)) {
    models <- list(models)
  }

  # model names
  if (is.null(names(models))) {
    model_names <- paste('Model', 1:length(models))
  } else {
    model_names <- names(models)
  }
  model_names <- pad(model_names)

  # tidy
  if (statistic == "conf.int") {
    ti <- lapply(models, function(x) 
                 get_estimates(x, conf_level=conf_level, ...))
  } else {
    ti <- lapply(models, function(x) 
                 get_estimates(x, ...))
  }

  # glance
  gl <- lapply(models, get_gof)

  # combine
  if (length(models) > 1) {
    for (i in seq_along(models)) {
      ti[[i]]$term <- paste(model_names[i], ti[[i]]$term)
      colnames(gl[[i]]) <- paste(model_names[i], colnames(gl[[i]]))
    }
  }

  gl <- bind_cols(gl)
  ti <- bind_rows(ti)

  # guess coef_group
  if (is.null(coef_group)) {
    if ("y.level" %in% colnames(ti)) {
      coef_group <- "y.level"
    } else if ("response" %in% colnames(ti)) {
      coef_group <- "response"
    } else if ("group" %in% colnames(ti)) {
      coef_group <- "group"
    } else {
      stop("You must specify a valid character value for the `coef_group` argument. To find this value for your type of model, load the `broom` and/or `broom.mixed` libraries, then call `tidy(model)` on your model object. The `coef_group` value must be a column in the resulting data.frame. This column includes identifiers which determine which coefficients appear in which columns of your table.") 
    }
  }

  # split by coef_group (treat them as separate models)
  results <- split(ti, ti[[coef_group]])

  # assemble modelsummary_list
  for (i in seq_along(results)) {
    results[[i]] <- list(tidy = results[[i]])
    class(results[[i]]) <- "modelsummary_list"
  }
  results[[1]]$glance <- gl

  modelsummary(results,
    output = output,
    fmt = fmt,
    conf_level = conf_level,
    stars = stars,
    coef_map = coef_map,
    coef_omit = coef_omit,
    coef_rename = coef_rename,
    gof_map = gof_map,
    gof_omit = gof_omit,
    statistic = statistic,
    statistic_override = statistic_override,
    add_rows = add_rows,
    title = title,
    notes = notes,
    estimate = estimate,
    statistic_vertical = statistic_vertical,
    ...) 

}
