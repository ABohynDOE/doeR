library(testthat)
library(doeR)       # load our package

test_that("Runsize must be a power of 2 between 16 and 4096.",{
  expect_error(
    design(8, "8-5.1"),
    "Runsize must be a power of 2 between 16 and 4096."
  )
})

test_that("Non-existing designs are not generated",{
  expect_error(
    design(32, "8-5.1"),
    'Design with runsize 32 and index "8-5.1" does not exist.'
  )
})
