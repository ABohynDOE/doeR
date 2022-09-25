test_that("Runsize must be a power of 2 between 16 and 4096.", {
  expect_error(
    design(8, "8-5.1"),
    "Runsize must be a power of 2 between 16 and 4096."
  )
})

test_that("Non-existing designs are not generated", {
  expect_error(
    design(32, "8-5.1"),
    'Design with runsize 32 and index "8-5.1" does not exist.'
  )
})

test_that("Designs with no column numbers in Xu are not generated", {
  expect_error(
    design(512, "46-37"),
    "Sorry, no columns given for that design. Impossible to generate !"
  )
})

test_that("Design model matrix cannot be created with negative k", {
  expect_error(
    design_model_matrix(-1, c(1, 2, 3)),
    "'k' must be a positive value."
  )
})
