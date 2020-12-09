#' internal function to build gof_map
#'
#' @keywords internal
gof_map_build <- function() {
  text <- 'raw, clean, fmt, omit,
  nobs,                      Num.Obs.,           0, FALSE,
  nimp,                      Num.Imp.,           0, FALSE,
  nclusters,                 Num.Clust.,         0, FALSE,
  nblocks,                   Num.Blocks,         0, FALSE,
  r.squared,                 R2,                 3, FALSE,
  r2,                        R2,                 3, FALSE,
  adj.r.squared,             R2 Adj.,            3, FALSE,
  r2.adjusted,               R2 Adj.,            3, FALSE,
  r2.marginal,               R2 Marg.,           3, FALSE,
  r2.conditional,            R2 Cond.,           3, FALSE,
  r2.within,                 R2 Within,          3, FALSE,
  r2.within.adjusted,        R2 Within Adj.,     3, FALSE,
  pseudo.r.squared,          R2 Pseudo,          3, FALSE,
  within.r.squared,          R2 Within,          3, FALSE,
  r.squared.within,          R2 Within,          3, FALSE,
  AIC,                       AIC,                1, FALSE,
  aic,                       AIC,                1, FALSE,
  BIC,                       BIC,                1, FALSE,
  bic,                       BIC,                1, FALSE,
  icc,                       ICC,                1, FALSE,
  logLik,                    Log.Lik.,           3, FALSE,
  std.error.type,            Std.Error Type,     3, FALSE,
  F,                         F,                  3, FALSE,
  deviance,                  Deviance,           2, TRUE,
  df.residual,               DF Resid,           0, TRUE,
  df.null,                   DF Null,            0, TRUE,
  sigma,                     Sigma,              3, TRUE,
  statistic,                 Statistics,         3, TRUE,
  p.value,                   p,                  3, TRUE,
  log.loss,                  Log Loss,           3, TRUE,
  pcp,                       PCP,                3, TRUE,
  r2.tjur,                   R2 tjur  ,          2, TRUE,
  r2.nagelkerke,             R2 Nagelkerke,      2, TRUE,
  score.log,                 Score Log,          3, TRUE,
  score.spherical,           Score Spherical,    3, TRUE,
  p.value,                   p,                  3, TRUE,
  df,                        DF,                 0, TRUE,
  null.deviance,             Deviance Null,      2, TRUE,
  rmse,                      RMSE,               2, TRUE,
  statistic.Weak.instrument, Weak IV F-stat,     1, FALSE,
  statistic.Wu.Hausman,      Wu-Hausman Chi-Sq., 1, FALSE,
  statistic.Sargan,          Sargan J-stat,      1, FALSE,
  p.value.Weak.instrument,   Weak IV p,          3, TRUE,
  p.value.Wu.Hausman,        Wu-Hausman p,       3, TRUE,
  p.value.Sargan,            Sargan p,           3, TRUE'
  out <- utils::read.csv(text=text, colClasses=c("character", "character", "numeric", "logical", "NULL"))
  for (i in 1:2) {
    out[[i]] <- trimws(out[[i]])
  }
  out
}

#' Data.frame used to clean up and format goodness-of-fit statistics
#'
#' By default, this data frame is passed to the 'gof_map' argument of the
#' 'modelsummary' function. Users can modify this data frame to
#' customize the list of statistics to display and their format. See example
#' below.
#' @docType data
#' @keywords datasets
#' @name gof_map
#' @format data.frame with 4 columns of character data: raw, clean, fmt, omit
#' @examples
#' \dontrun{
#'
#' library(modelsummary)
#' mod <- lm(wt ~ drat, data = mtcars)
#' gm <- modelsummary::gof_map
#' gm$omit[gm$raw == 'deviance'] <- FALSE
#' gm$fmt[gm$raw == 'r.squared'] <- "%.5f"
#' modelsummary(mod, gof_map = gm)
#' }
#'
#' @export
gof_map <- gof_map_build() 
