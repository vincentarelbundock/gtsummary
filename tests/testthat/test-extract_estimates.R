context('coef_map')

library(modelsummary)

test_that("bad estimate name throws error", {
  mod <- lm(am ~ drat, data = mtcars)
  expect_error(modelsummary:::extract_estimates(mod, estimate = "junk"))
})

test_that("bad statistic name throws error", {
  mod <- lm(am ~ drat, data = mtcars)
  expect_error(
    modelsummary(mod, output="dataframe", statistic = "junk")
  )
  expect_error(
    modelsummary(models, 
                 statistic="junk",
                 output="dataframe",
                 statistic_override = vcov)
  )
})
