context("estimate_override")

library(modelsummary)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)
dat$Clergy <- ifelse(dat$Clergy > 40, 1, 0) # binary variable for logit model
models <- list()
models[['OLS 1']] <- lm(Literacy ~ Crime_prop + Infants, dat)
models[['Poisson 1']] <- glm(Literacy ~ Crime_prop + Donations, dat, family = poisson())
models[['OLS 2']] <- lm(Desertion ~ Crime_prop + Infants, dat)
models[['Poisson 2']] <- glm(Desertion ~ Crime_prop + Donations, dat, family = poisson())
models[['Logit 1']] <- glm(Clergy ~ Crime_prop + Infants, dat, family = binomial())

test_that("single model", {
  mod <- lm(hp ~ mpg + drat, mtcars)
  x <- modelsummary(mod, estimate_override=vcov, output="data.frame")
  y <- modelsummary(mod, estimate_override=vcov(mod), output="data.frame")
  z <- modelsummary(mod, estimate_override=sqrt(diag(vcov(mod))), output="data.frame")
  expect_equal(x, y)
  expect_equal(y, z)
})

# reference tables
results <- list()

results[['one sandwich']] <- modelsummary(
  models,
  output = "data.frame",
  estimate_override = vcov,
  estimate = c("estimate", "p.value"),
  fmt = "%.7f")

results[['many sandwiches']] <- modelsummary(
  models,
  output = "data.frame",
  estimate_override = list(vcov, vcov, vcov, vcov, vcov),
  fmt = "%.7f")

results[['list of matrices']] <- modelsummary(
  models,
  output = "data.frame",
  estimate_override = lapply(models, vcov),
  fmt = "%.7f")

results[['hardcoded numerical']] <- modelsummary(
  models,
  output = "data.frame",
  fmt = "%.7f",
  estimate_override = list(
    `OLS 1` = c('(Intercept)' = 2, Crime_prop = 3, Infants = 4),
    `NBin 1` = c('(Intercept)' = 3, Crime_prop = -5, Donations = 3),
    `OLS 2` = c('(Intercept)' = 7, Crime_prop = -6, Infants = 9),
    `NBin 2` = c('(Intercept)' = 4, Crime_prop = -7, Donations = -9),
    `Logit 1` = c('(Intercept)' = 1, Crime_prop = -5, Infants = -2)))

results[['hardcoded arbitrary']] <- modelsummary(
  models,
  output = "data.frame",
  fmt = "%.7f",
  estimate_override = list(
    `OLS 1` = c('(Intercept)' = "!!", Crime_prop = "!!!", Infants = "!!!!"),
    `NBin 1` = c('(Intercept)' = "[1, 5]", Crime_prop = "[-5, +5]", Donations = "95% CI[-2, -1]"),
    `OLS 2` = c('(Intercept)' = "7", Crime_prop = "-6", Infants = "9"),
    `NBin 2` = c('(Intercept)' = "\U03B5", Crime_prop = "\U2135", Donations = "\U0414"),
    `Logit 1` = c('(Intercept)' = 1, Crime_prop = -5, Infants = -2)) )

# we are not interested in GOFs in this test
for (i in seq_along(results)) {
  results[[i]] <- results[[i]][results[[i]]$part == "estimates",, drop=FALSE]
  results[[i]]$part <- results[[i]]$statistic <- NULL
}

# # save known values (comment out until new manual check)
# saveRDS(results, file="known_output/statistic-override.rds")

# load reference values (comment out when updating)
reference <- readRDS(file = "known_output/statistic-override.rds")


test_that("bad function", {
  expect_error(modelsummary(models, estimate_override = na.omit))
})

test_that("vector must be named", {
  vec <- as.numeric(1:3)
  expect_error(modelsummary(models[[1]], estimate = c("estimate", "std.error"), estimate_override = vec))
})

test_that("estimate_override content", {
  expect_equivalent(results[["one sandwich"]], reference[["one sandwich"]])
  expect_equivalent(results[["many sandwiches"]], reference[["many sandwiches"]])
  expect_equivalent(results[["list of matrices"]], reference[["list of matrices"]])
  expect_equivalent(results[["hardcoded numerical"]], reference[["hardcoded numerical"]])
  expect_equivalent(results[["hardcoded arbitrary"]], reference[["hardcoded arbitrary"]])
})

test_that("useless: function but no ci needed", {
  expect_error(modelsummary(models, estimate_override=vcov, conf_level=NULL), NA)
})
