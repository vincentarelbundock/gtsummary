# context('mice')

# # load
# library(modelsummary)
# library(mice)

# # impute
# set.seed(10393983)
# dat <- mtcars
# dat$wt[sample(1:nrow(dat), 4)] <- NA
# dat$hp[sample(1:nrow(dat), 3)] <- NA
# dat <- mice(dat, m = 5, printFlag = FALSE, seed = 10)

# # fit
# f <- am ~ wt + hp
# mod <- list()
# mod$OLS <- with(dat, lm(am ~ wt + hp + vs))
# mod$Logit <- with(dat, glm(am ~ wt + vs, family = binomial()))
# mod <- lapply(mod, mice::pool)

# # test
# test_that("ols and logit", {

#   raw <- modelsummary(mod, output="dataframe")

#   truth <-   c("1.702", "(0.357)", "-0.455", "(0.101)", "0.001", "(0.002)", "-0.102", "(0.199)", "32", "5", "0.543", "0.494")
#   expect_equal(truth, unname(raw[[4]]))

#   truth <- c("20.500", "(8.584)", "-6.273", "(2.511)", "", "", "-3.816", "(2.482)", "32", "5", "", "")
#   expect_equal(truth, unname(raw[[5]]))

# })
