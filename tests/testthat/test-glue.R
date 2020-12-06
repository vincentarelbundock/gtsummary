context("glue")

library(modelsummary)

mod <- list(
  lm(mpg ~ hp, data=mtcars),
  glm(vs ~ hp + cyl, data=mtcars, family=binomial))

test_that("glue + no statistic",{
  tab <- modelsummary(
    mod, 
    estimate  = "**{estimate}** [{conf.low}, {conf.high}] ({p.value})",
    statistic = NULL,
    output = "data.frame",
    gof_omit  = ".*")
  truth4 <- c("**30.099** [26.762, 33.436] (0.000)", "**-0.068** [-0.089, -0.048] (0.000)", "")
  truth5 <-  c("**9.089** [4.102, 18.413] (0.006)", "**-0.041** [-0.139, 0.013] (0.255)", 
               "**-0.690** [-2.499, 0.795] (0.382)")
  expect_equal(tab[[4]], truth4)
  expect_equal(tab[[5]], truth5)
})

test_that("glue + multi statistics",{
  tab <- modelsummary(
    mod, 
    estimate  = "**{estimate}** [{conf.low}, {conf.high}]",
    statistic = c("t={statistic}", 
                  "p={p.value}"),
    output = "data.frame",
    gof_omit  = ".*")
  truth4 <- c("**30.099** [26.762, 33.436]", "t=18.421", "p=0.000", "**-0.068** [-0.089, -0.048]", 
              "t=-6.742", "p=0.000", "", "", "")
  truth5 <- c("**9.089** [4.102, 18.413]", "t=2.759", "p=0.006", "**-0.041** [-0.139, 0.013]", 
              "t=-1.139", "p=0.255", "**-0.690** [-2.499, 0.795]", "t=-0.873", "p=0.382")
  expect_equal(tab[[4]], truth4)
  expect_equal(tab[[5]], truth5)
})
