context('fixest')

library(modelsummary)
library(fixest)

test_that("fixest", {
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- modelsummary(mod, "data.frame")
  expect_S3_class(raw, "data.frame")
})

test_that("fixest glance_custom", {
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- glance_custom(mod)
  expect_is(raw, "data.frame")
  expect_equal(dim(raw), c(1, 2))
})
