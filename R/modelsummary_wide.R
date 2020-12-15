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
  stacking = "horizontal",
  title = NULL,
  notes = NULL,
  estimate = "estimate",
  statistic_vertical = NULL,
  ...) {

  checkmate::assert_character(stacking, pattern="^horizontal$|^vertical$")

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
  gl_wide <- gl

  # insert model names in glance and tidy frames
  for (i in seq_along(model_names)) {
    ti[[i]]$model <- model_names[i]
    gl[[i]]$model <- model_names[i]
    if (i == 1) {
      colnames(gl_wide[[i]]) <- paste(model_names[i], colnames(gl_wide[[i]]))
    }
  }
  ti <- bind_rows(ti)
  gl <- bind_rows(gl)
  gl_wide <- bind_cols(gl_wide)

  # guess coef_group
  if (is.null(coef_group)) {
    coef_group <- intersect(c("y.level", "response", "group"), colnames(ti))[1]
    if (is.na(coef_group)) {
      stop("You must specify a valid character value for the `coef_group` argument. To find this value for your type of model, load the `broom` and/or `broom.mixed` libraries, then call `tidy(model)` on your model object. The `coef_group` value must be a column in the resulting data.frame. This column includes identifiers which determine which coefficients appear in which columns of your table.") 
    }
  }

  # unique group names
  group_names <- unique(ti[[coef_group]])

  # vertical stacking: model_names are groups
  if (stacking == "vertical") {
    results <- list()
    for (g in group_names) {
      results[[g]]$tidy <- ti[ti[[coef_group]] == g, , drop=FALSE]
      results[[g]]$tidy$term <- paste(results[[g]]$tidy$model,
                                      results[[g]]$tidy$term)
      if (g == group_names[1]) {
        results[[g]]$glance = gl_wide
      }
      class(results[[g]]) <- c("modelsummary_list", "list")
    }
  }
      
  # horizontal stacking: model_names are model/group combinations
  if (stacking == "horizontal") {
    results <- list()
    for (m in model_names) {
      for (g in group_names) {
        results[[paste(m, g)]]$tidy <- ti[ti[[coef_group]] == g & ti$model == m, , drop=FALSE]
        if (g == group_names[1]) {
          results[[paste(m, g)]]$glance <- gl[gl$model == m,]
          results[[paste(m, g)]]$glance$model <- NULL # otherwise "model" appears in the table
        }
        class(results[[paste(m, g)]]) <- c("modelsummary_list", "list")
      }
    }
  }

  # output
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
