# context("modelsummary_wide")

# library(nnet)

# test_that("no group", {
#   mod <- list(
#     lm(hp ~ mpg, mtcars),
#     lm(hp ~ mpg + drat + vs, mtcars)
#   )
#   expect_error(modelsummary_wide(mod))
# })


# test_that("nnet::multinom one model", {
#   var1 <- sample(c('A', 'B', 'C'), replace = T, size=100)
#   var2 <- sample(c(0,1), size=100, replace=T)
#   var3 <- rnorm(100, mean=10, sd=2)
#   df <- data.frame(var1, var2, var3)
#   invisible(capture.output(mod <- nnet::multinom(var1~var2, data=df)))
#   tmp <- modelsummary_wide(mod, output="data.frame")
#   expect_is(tmp, "data.frame")
#   expect_equal(dim(tmp), c(7, 5))
# })


# test_that("nnet::multinom two models", {

#   var1 <- sample(c('A', 'B', 'C'), replace = T, size=100)
#   var2 <- sample(c(0,1), size=100, replace=T)
#   var3 <- rnorm(100, mean=10, sd=2)
#   df1 <- data.frame(var1, var2, var3)

#   var1 <- sample(c('A', 'B', 'C'), replace = T, size=100)
#   var2 <- sample(c(0,1), size=100, replace=T)
#   var3 <- rnorm(100, mean=10, sd=2)
#   df2 <- data.frame(var1, var2, var3)

#   invisible(capture.output(m1 <- nnet::multinom(var1~var2, data=df1)))
#   invisible(capture.output(m2 <- nnet::multinom(var1~var2, data=df2)))

#   tmp <- modelsummary_wide(list(m1, m2), output="data.frame")
#   expect_is(tmp, "data.frame")
#   expect_equal(dim(tmp), c(16, 5))

# })
