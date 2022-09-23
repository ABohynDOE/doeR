library(testthat) # load testthat package
library(doeR) # load our package

test_that("num2gen() returns the correct values", {
  generators <- num2gen(c(7, 13, 8))
  expect_equal(generators, c("abc", "acd", "d"))
})

test_that("num2gen() returns a vector of the correct length", {
  expect_length(num2gen(7), 1)
  expect_length(num2gen(c(7, 13, 16)), 3)
})
