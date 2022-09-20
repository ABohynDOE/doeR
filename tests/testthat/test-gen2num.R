library(testthat)   # load testthat package
library(doeR)       # load our package

test_that("gen2() returns the correct values", {
  numbers <- gen2num(c("abc","acd","e"))
  expect_equal(numbers, c(7,13,16))
})

test_that("gen2num() does not work with non lower-case letters",{
  expect_error(gen2num('Abc'), "Generators can only contain lowercase letters")
})
