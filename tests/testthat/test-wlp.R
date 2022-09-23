test_that("Hamming distance gives an error for vectors of different sizes", {
  a <- c(0,1,1,1,2)
  b <- c(0,1,1,1)
  expect_error(hamming_distance(a,b), "Both vectors must have the same size")
})

test_that("Hamming distance works", {
  a <- c(0,1,0,1,0,1)
  b <- c(0,1,1,0,0,1)
  # Difference between the two should be 2
  expect_equal(hamming_distance(a, b), 2)
})

test_that("Distance distribution has correct length", {
  a <- c(0,1,0,1,1,0)
  m <- matrix(rep(a,5), nrow=5, byrow=T)
  expect_length(distance_distribution(m), length(a)+1)
})

test_that("Distance distribution has correct length", {
  m <- matrix(0, nrow=8, ncol=7)
  for (i in c(1:8)) {
    m[i,] = round(runif(7))
  }
  expect_lte(distance_distribution(m)[1], 8)
})
