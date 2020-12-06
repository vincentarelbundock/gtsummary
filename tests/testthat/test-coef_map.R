context('coef_map')

library(modelsummary)
library(tibble)

test_that("combine different regressors and collapse rows", {
  cmap <- c('(Intercept)' = 'Constant', 'drat' = 'Combined', 'qsec' = 'Combined')
  mod <- list()
  mod$OLS <- lm(am ~ drat, data = mtcars)
  mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())
  raw <- modelsummary(mod, output = "data.frame", coef_map = cmap)
  truth <- c('Constant', 'Constant', 'Combined', 'Combined', 'Num.Obs.')
  expect_equal(unname(raw[[2]][1:5]), truth)
})

test_that("reorder and omit", {
  cmap <- c('qsec' = 'qsec', 'drat' = 'drat')
  mod <- list()
  mod$OLS <- lm(am ~ drat, data = mtcars)
  mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())
  raw <- modelsummary(mod, coef_map = cmap, output="dataframe")
  truth <- c('qsec', 'qsec', 'drat', 'drat' , 'Num.Obs.')
  expect_equal(unname(raw[[2]][1:5]), truth)
})

test_that("coef_map with multiple vertical statistics", {

  cm <- c("(Intercept)" = "Intercept",
    "factor(cyl)6" = "6-cylinder",
    "factor(cyl)8" = "8-cylinder")
  models <- list()
  models[['OLS']] <- lm(mpg ~ factor(cyl), mtcars)
  models[['Logit']] <- glm(am ~ factor(cyl), mtcars, family = binomial)

  mat <- modelsummary(models, coef_map=cm, output="dataframe")
  expect_s3_class(mat, 'data.frame')
  expect_equal(dim(mat), c(13, 5))

  mat <- modelsummary(
    models,
    output="dataframe",
    statistic = c('std.error', 'conf.int'),
    coef_map = cm
  )
  expect_s3_class(mat, 'data.frame')
  expect_equal(dim(mat), c(16, 5))

  rows <- tibble::tribble(
    ~term, ~OLS, ~Logit,
    '4-cylinder', '-', '-',
    '12-cylinder', '-', '-')

  mat <- modelsummary(models,
    output = 'dataframe',
    statistic = c('std.error', 'conf.int'),
    add_rows = rows,
    coef_map = cm)

  expect_s3_class(mat, 'data.frame')
  expect_equal(dim(mat), c(18, 5))

})
