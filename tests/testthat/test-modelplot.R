library(ggplot2)
library(sandwich)

context("modelplot")

# CRAN requires vdiffr to be conditional
# Function recommended by Lionel Henry on 2020-12-08
expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

test_that("single model", {

  mod <- lm(hp ~ mpg + drat, data = mtcars)
  p <- modelplot(mod)
  expect_doppelganger("vanilla", p)

  mod <- lm(hp ~ mpg + drat, data = mtcars)
  p <- modelplot(mod, coef_omit = 'Interc')
  expect_doppelganger("coef_omit", p)

  params <- list(
    geom_vline(xintercept = 0, color = 'orange'),
    annotate("rect", alpha = .1, xmin = -.5, xmax = .5, ymin = -Inf, ymax = Inf),
    geom_point(aes(y = term, x = estimate), alpha = .3,
      size = 10, color = 'red', shape = 'square')
  )
  p <- modelplot(mod,
    coef_map = c("drat" = "Rear axle ratio", "mpg" = "Miles / gallon"),
    color = "green", shape = "square", background = params)
  expect_doppelganger("coef_map + color + shape + background", p)

})

test_that("multiple models", {

  mod <- list(lm(hp ~ mpg + drat, data = mtcars),
    lm(hp ~ mpg, data = mtcars))
  p <- modelplot(mod)
  expect_doppelganger("multiple plots vanilla", p)

  mod <- list(lm(hp ~ mpg + drat, data = mtcars),
    lm(hp ~ mpg, data = mtcars))
  p <- modelplot(mod, facet = TRUE)
  expect_doppelganger("multiple plots facet", p)

})

test_that("conf_level=NULL", {
  mod <- lm(hp ~ mpg + drat, data = mtcars)
  p <- modelplot(mod, draw = FALSE, conf_level = NULL)
  expect_is(p, "data.frame")
  expect_equal(nrow(p), 3)
})


test_that("statistic_override", {
  mod <- list(lm(hp ~ mpg + drat, data = mtcars),
              lm(hp ~ mpg + drat, data = mtcars))
  so <- list(vcov, sandwich::vcovHC)
  p <- modelplot(mod, statistic_override=so, draw=FALSE)
  known <- c(-22.1832974370101, -28.1858724755655, -13.6502180401172, -15.0390897152002, 165.179327669237, 182.406373565932)         
  expect_equal(p$conf.low, known)
})


# TODO: these tests are too minimalist

test_that("single model without ci", {
  mod <- lm(hp ~ mpg + drat, data = mtcars)
  expect_error(modelplot(mod, conf_level=NULL), NA)
})

test_that("multiple models without ci", {
  mod <- list(
    lm(hp ~ mpg, data = mtcars),
    lm(hp ~ mpg + drat, data = mtcars),
    lm(hp ~ mpg + drat + am, data = mtcars)
  )
  expect_error(modelplot(mod, facet=TRUE, conf_level=NULL), NA)
  expect_error(modelplot(mod, facet=FALSE, conf_level=NULL), NA)
})
