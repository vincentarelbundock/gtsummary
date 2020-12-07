context('ellipses')

library(modelsummary)

test_that("exponentiate logit coefficients", {

  mod <- glm(am ~ mpg, mtcars, family = binomial)

  # not exponentiated
  raw <- modelsummary(
    mod, 
    gof_omit=".*",
    output="dataframe", 
    exponentiate=FALSE)
  truth <- c("-6.604", "(2.351)", "0.307", "(0.115)")
  expect_equal(raw[[4]], truth)

  # exponentiated conf.int
  raw <- modelsummary(
    mod, 
    gof_omit=".*",
    statistic="conf.int",
    output="dataframe", 
    exponentiate=FALSE)
  truth <- c("-6.604", "[-12.328, -2.772]", "0.307", "[0.122, 0.587]")
  expect_equal(raw[[4]], truth)

  # exponentiated std.error
  # As noted by Alex P Hayes: 
  # https://github.com/tidymodels/broom/issues/422
  # I believe this is the correct behavior: quantiles (and thereby confidence
  # intervals) are invariant under monotonic transformation (exp()) but
  # variances are not. 
  raw <- modelsummary(
    mod, 
    gof_omit=".*",
    output="dataframe", 
    exponentiate=FALSE)
  truth <- c("-6.604", "(2.351)", "0.307", "(0.115)")
  expect_equal(raw[[4]], truth)

})
