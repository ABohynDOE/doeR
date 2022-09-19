library(testthat)   # load testthat package
library(doeR)       # load our package

test_that("num2gen() returns the correct values", {
  generators <- num2gen(c(7,13,8))
  expect_equal(generators, c('abc','acd','d'))
})
