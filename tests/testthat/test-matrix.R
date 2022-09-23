test_that("Small basic factor matrix is correct", {
  mat <- matrix(data = 0, nrow = 4, ncol = 2)
  for (i in c(3,4)) {
    mat[i,1] = 1
  }
  for (i in c(2,4)){
    mat[i,2] = 1
  }
  expect_equal(basic_factor_matrix(2), mat)
})

test_that("Mode matrix for basic factors is correct", {
  mat <- diag(4)
  expect_equal(design_model_matrix(4, c(1,2,4,8)), mat)
})
