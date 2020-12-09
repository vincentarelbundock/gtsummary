context('fixest')

library(modelsummary)
library(fixest)

test_that("fixest", {
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- modelsummary(mod, "data.frame")
  expect_s3_class(raw, "data.frame")
  expect_equal(dim(raw), c(14, 4))
})

test_that("fixest glance_custom", {
  mod <- feols(Sepal.Length ~ Sepal.Width + Petal.Length | Species, iris)
  raw <- glance_custom(mod)
  expect_is(raw, "data.frame")
  expect_equal(dim(raw), c(1, 2))
})
