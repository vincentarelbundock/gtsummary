context('statistic')

library(modelsummary)

mod <- list()
mod$OLS <- lm(am ~ drat, data = mtcars)
mod$Logit <- glm(am ~ qsec, data = mtcars, family = binomial())

test_that("std.error", {
  raw <- modelsummary(mod, statistic = 'std.error', output="dataframe")
  truth <- c('(0.434)', '(0.120)')
  expect_equal(truth, unname(raw[[4]][c(2, 4)]))
  truth <- c('(4.045)', '(0.228)')
  expect_equal(truth, unname(raw[[5]][c(2, 6)]))
})

test_that("p.value", {
  raw <- modelsummary(mod, statistic = 'p.value', fmt = '%.6f', output="dataframe")
  truth <- c('(0.000078)', '(0.000005)')
  expect_equal(truth, unname(raw[[4]][c(2, 4)]))
  truth <- c('(0.241402)', '(0.206028)')
  expect_equal(truth, unname(raw[[5]][c(2, 6)]))
})

test_that("conf.int", {
  raw <- modelsummary(mod, statistic = 'conf.int', output="dataframe")
  truth <- c("[-2.873, -1.099]", "[0.421, 0.909]")
  expect_equal(truth, unname(raw[[4]][c(2, 4)]))
  truth <- c("[-2.760, 13.501]", "[-0.784, 0.131]")
  expect_equal(truth, unname(raw[[5]][c(2, 6)]))
})

test_that("conf.int, conf_level = 0.99", {
  raw <- modelsummary(mod, statistic = 'conf.int', conf_level = .99, output="dataframe")
  truth <- c("[-3.181, -0.791]", "[0.336, 0.994]")
  expect_equal(truth, unname(raw[[4]][c(2, 4)]))
  truth <- c("[-5.070, 16.689]", "[-0.966, 0.259]")
  expect_equal(truth, unname(raw[[5]][c(2, 6)]))
})
