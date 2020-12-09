#' Extract goodness-of-fit statistics from a single model
#' @param model object type with an available `glance` method.
#' @inheritParams modelsummary
#' @return data.frame with goodness-of-fit  statistics
#' @keywords internal
extract_gof <- function(model, fmt, gof_map, ...) {

  # extract gof from model object
  gof <- get_gof(model, ...)
 
  # lm model: include F-stat by default
  if (isTRUE(class(model)[1] == "lm")) { # glm also inherits from lm
    if (inherits(gof, "performance_model")) {
      gof$F <- attr(gof, "r2")$F
    } else {
      gof$F <- gof$statistic
    }
  }

  # glance_custom
  gof_custom <- glance_custom(model)
  sanity_gof(gof, gof_custom)
  if (!is.null(gof_custom)) {
    for (n in colnames(gof_custom)) {
      gof[[n]] <- gof_custom[[n]]
    }
  }

  # define gof_map if not supplied. Keep anything not included explicitly
  if (is.null(gof_map)) {

    gof_map <- modelsummary::gof_map
    # drop if gof_map$omit == TRUE
    bad <- gof_map$raw[gof_map$omit]
    gof <- gof[setdiff(colnames(gof), bad)]

  # used supplied gof_map. Drop anything not included explicitly
  } else {
    # keep only if gof_map$omit == FALSE
    good <- gof_map$raw[!gof_map$omit]
    gof <- gof[intersect(colnames(gof), good)]
  }

  # re-order gof columns
  idx1 <- intersect(gof_map$raw, colnames(gof))
  idx2 <- setdiff(colnames(gof), gof_map$raw)
  gof <- gof[c(idx1, idx2)]

  # if number of gof > 0
  if (ncol(gof) > 0) {

    for (i in seq_along(gof)) {

      idx <- match(colnames(gof)[i], gof_map$raw)

      if (!is.na(idx)) { # if gof in gof_map

        # rename
        colnames(gof)[i] <- gof_map$clean[idx]

        # round integer/numeric values
        if (inherits(gof[[i]], 'numeric')) {
          gof[[i]] <- rounding(gof[[i]], gof_map$fmt[idx])
        } else {
          gof[[i]] <- as.character(gof[[i]])
        }

      } else { # if gof is not in gof_map

        # round integer/numeric values
        if (inherits(gof[[i]], 'numeric')) {
          gof[[i]] <- rounding(gof[[i]], fmt)
        }
        else {
          gof[[i]] <- as.character(gof[[i]])
        }
      }
    }

    # reshape
    out <- data.frame(term = names(gof), value = unlist(gof))

  } else { # all gof are excluded return an empty tibble (needs character to match merge type)
    out <- data.frame(term = NA_character_, value = NA_character_)
    out <- stats::na.omit(out)
  }

  # factor to character (otherwise gof_map breaks under R < 4.0.0)
  for (i in seq_along(out)) {
    if (is.factor(out[[i]])) {
      out[[i]] <- as.character(out[[i]])
    }
  }

  # output
  return(out)
}

get_gof <- function(model, ...) {

  flag <- function(x) {
    inherits(x, "data.frame") &&
    nrow(x) == 1
  }

  # if a `glance` method is loaded in memory use that as default
  gof <- suppressWarnings(try(
    generics::glance(model, ...), silent=TRUE))
  if (flag(gof)) return(gof)

  # easystats/performance is the default
  gof <- suppressWarnings(try(
    glance_easystats(model, ...), silent=TRUE))
  if (flag(gof)) return(gof)

  # if broom is installed, try it
  if (check_dependency("broom")) {
    gof <- suppressWarnings(try(
      broom::glance(model, ...), silent=TRUE))
    if (flag(gof)) return(gof)
  }

  # if broom.mixed is installed, try it
  if (check_dependency("broom.mixed")) {
    gof <- suppressWarnings(try(
      broom.mixed::glance(model, ...), silent=TRUE))
    if (flag(gof)) return(gof)
  }

  stop(sprintf('Cannot extract information from models of class "%s". Consider installing and loading the `parameters`, `performance`, and `broom.mixed` or any other package with `tidy` and `glance` functions appropriate for this model type. Alternatively, you can define your own `tidy` method, following the instructions on the `modelsummary` website: https://vincentarelbundock.github.io/modelsummary/articles/newmodels.html', class(model)[1]))

}
 
