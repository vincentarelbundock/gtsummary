#' Extract estimates and statistics from a single model
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return data.frame with side-by-side model summaries
#' @keywords internal
extract_estimates <- function(model,
                              statistic = 'std.error',
                              statistic_override = NULL,
                              statistic_vertical = TRUE,
                              conf_level = .95,
                              fmt = '%.3f',
                              stars = FALSE,
                              estimate = 'estimate',
                              ...) {

  # convert estimate/statistic to glue format
  estimate_glue <- ifelse(
    grepl("\\{", estimate),
    estimate, 
    sprintf("{%s}", estimate))

  if (is.null(statistic)) {
    statistic_glue <- estimate_glue
  } else {
    statistic_glue <- ifelse(
      statistic == "conf.int",
      "[{conf.low}, {conf.high}]",
      statistic)
    if (is.null(statistic_override) || !is.character(statistic_override)) {
      statistic_glue <- ifelse(
        grepl("\\{", statistic_glue),
        statistic_glue,
        sprintf("({%s})", statistic))
    } else { # no automatic parentheses if user-supplied characters
      statistic_glue <- ifelse(
          grepl("\\{", statistic_glue),
          statistic_glue,
          sprintf("{%s}", statistic))
    }
  } 

  # extract estimates using broom or parameters
  if (any(grepl("conf", statistic)) | any(grepl("conf", estimate))) {
    est <- suppressWarnings(try(tidy(model, conf.int=TRUE, conf.level=conf_level, ...), silent=TRUE))
    if (!inherits(est, "data.frame") || nrow(est) == 0) {
      est <- suppressWarnings(try(tidy_easystats(model, ci=conf_level, ...), silent=TRUE))
    }
  } else {
    est <- suppressWarnings(try(tidy(model, ...), silent=TRUE))
    if (!inherits(est, "data.frame") || nrow(est) == 0) {
      est <- suppressWarnings(try(tidy_easystats(model, ...), silent=TRUE))
    }
  } 

  if (!inherits(est, "data.frame") || nrow(est) == 0 | !"term" %in% colnames(est)) {
    stop(sprintf('Cannot extract the required information from models of class "%s". Consider installing and loading `broom.mixed`, or any other package that includes `tidy` and `glance` methods appropriate for this model type. Alternatively, you can easily define your own methods by following the instructions on the `modelsummary` website: https://vincentarelbundock.github.io/modelsummary/articles/newmodels.html', class(model)[1]))
  }

  # statistic override
  if (!is.null(statistic_override)) {

    # extract overriden statistics
    so <- extract_statistic_override(
      model,
      statistic_override=statistic_override,
      conf_level=conf_level)

    # keep only columns that do not appear in so
    est <- est[, c('term', base::setdiff(colnames(est), colnames(so))), drop = FALSE]

    # make sure statistic_override is of the correct length
    if (nrow(est) != nrow(so)) {
      stop("statistic_override and estimates have different dimensions.")
    }

    # merge statistic_override and estimates
    est <- merge(est, so, by="term", sort=FALSE)

    # # sanity
    # bad1 <- (statistic != "conf.int") & (!statistic %in% colnames(so))
    # bad2 <- (statistic == "conf.int") & (!"conf.low" %in% colnames(so))
    # if (bad1 || bad2) {
    #   stop(paste0(statistic, " cannot be extracted through the `statistic_override` argument. You might want to install the `lmtest` package and/or look at the `modelsummary:::extract_statistic_override` function to diagnose the problem."))
    # } 

  }

  # # tidy_custom
  # est_custom <- tidy_custom(model)
  # sanity_tidy(est, est_custom, estimate, statistic, class(model)[1])
  # if (!is.null(est_custom)) {
  #   for (n in colnames(est_custom)) {
  #     est[[n]] <- est_custom[[n]]
  #   }
  # }

  # group coefficients
  if (anyDuplicated(est$term) > 0) {
    # broom.mixed `group` column
    if ("group" %in% colnames(est)) {
      est$term <- ifelse(is.na(est$group), est$term, paste(est$group, est$term))
    }
    # nnet::multinom `y.level` column
    if ("y.level" %in% colnames(est)) {
      est$term <- ifelse(is.na(est$y.level), est$term, paste(est$y.level, est$term))
    }
  }

  # round everything
  for (n in colnames(est)) {
    est[[n]] <- rounding(est[[n]], fmt)
  }

  # stars
  if (!isFALSE(stars)) {
    if (!'p.value' %in% colnames(est)) {
      stop('To use the `stars` argument, the `tidy` function must produce a column called "p.value"')
    }
    est[[estimate]] <- make_stars(est[[estimate]], 
                                  est$p.value,
                                  stars)
  }

  # extract statistics (there can be several)
  for (i in seq_along(statistic_glue)) {
    s <- statistic_glue[i]
    est[[paste0("statistic", i)]] <- as.character(glue::glue_data(est, s))

    # avoid empty parentheses for NAs
    est[[paste0("statistic", i)]][est[[s]] == ""] <- ""
  }

  # extract estimates
  est$estimate <- as.character(glue::glue_data(est, estimate_glue))

  # subset columns
  cols <- c('term', 'estimate', paste0('statistic', seq_along(statistic_glue)))
  est <- est[, cols, drop = FALSE]

  # reshape to vertical
  # sort by statistic to have se below coef, but don't sort terms
  # use factor hack to preserve order with weird names like cor__(Inter.)
  # especially important for broom.mixed models 
  est$term <- factor(est$term, unique(est$term))
  est <- stats::reshape(
    data.frame(est),
    varying       = grep("estimate|statistic", colnames(est), value=TRUE),
    times         = grep("estimate|statistic", colnames(est), value=TRUE),
    v.names       = "value",
    timevar       = "statistic",
    direction     = "long",
    new.row.names = 1:1000)
  est$id <- NULL

  # sort then convert back to character
  est <- est[order(est$term, est$statistic),]
  est$term <- as.character(est$term)

  # drop empty rows (important for broom.mixed which produces group
  # estimates without standard errors)
  est <- est[est$value != "", ]

  # drop statistic if NULL
  if (is.null(statistic)) {
    est <- est[!grepl("statistic", est$statistic), , drop=FALSE]
  }

  # output
  return(est)
}
