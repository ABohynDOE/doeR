## HAMMING DISTANCE
test_that("Hamming distance gives an error for vectors of different sizes", {
  a <- c(0, 1, 1, 1, 2)
  b <- c(0, 1, 1, 1)
  expect_error(hamming_distance(a, b), "Both vectors must have the same size")
})

test_that("Hamming distance works", {
  a <- c(0, 1, 0, 1, 0, 1)
  b <- c(0, 1, 1, 0, 0, 1)
  # Difference between the two should be 2
  expect_equal(hamming_distance(a, b), 2)
})


## DISTANCE DISTRIBUTION
test_that("Distance distribution has correct length", {
  a <- c(0, 1, 0, 1, 1, 0)
  m <- matrix(rep(a, 5), nrow = 5, byrow = T)
  expect_length(distance_distribution(m), length(a) + 1)
})

test_that("Distance distribution has correct length", {
  m <- matrix(0, nrow = 8, ncol = 7)
  for (i in c(1:8)) {
    m[i, ] <- round(runif(7))
  }
  expect_lte(distance_distribution(m)[1], 8)
})

## FACTORIAL
test_that("Factorial doesn't work for - numbers", {
  expect_error(factorial(-1, "Undefined behavior for negative numbers."))
})

test_that("Factorial", {
  expect_equal(factorial(5), 5 * 4 * 3 * 2 * 1)
})

## NCHOOSEK
test_that("Binomial for negative k or n<k", {
  n <- sample(5:20, 1)
  expect_equal(nchoosek(n, -1), 0)
})

test_that("Binomial for k == 0", {
  n <- sample(5:20, 1)
  expect_equal(nchoosek(n, 0), 1)
})

test_that("Binomial", {
  expect_equal(nchoosek(8, 3), 56)
})

## KRAWTCHOUK
test_that("Krawtchouck is stable for j=0", {
  x <- sample(1:20, 1)
  n <- sample(1:20, 1)
  expect_equal(krawtchouk(0, x, n), 1)
})

test_that("Krawtchouk (using formulas from Wikipedia definition)", {
  n <- sample(1:20, 1)
  x <- sample(1:n, 1)
  j <- sample(1:2, 1)
  if (j == 1) {
    res <- -2 * x + n
  }
  if (j == 2) {
    res <- 2 * x**2 - 2 * n * x + n * (n - 1) / 2
  }
  expect_equal(krawtchouk(j, x, n), res)
})

## MAC WILLIAMS
test_that("MacWilliams transform", {
  d <- c(1.0, 0.0, 1.0, 10.0, 11.0, 4.0, 3.0, 2.0, 0.0)
  mw_d <- c(1.0, 0.0, 0.0, 0.0, 3.0, 4.0, 0.0, 0.0, 0.0)
  expect_equal(mac_williams_transform(d, 32), mw_d)
})


## WLP
test_that("WLP with a random design from CSW", {
  data("csw")
  random_row <- sample(1:dim(csw)[1], 1)
  nruns <- csw[random_row, ]$n.runs
  cols <- csw[random_row, ]$cols
  numcols <- sapply(strsplit(cols, ",")[[1]], as.numeric)
  wlp <- csw[random_row, ]$wlp
  numwlp <- as.vector(sapply(strsplit(wlp, ",")[[1]], as.numeric))
  if (nruns == 64) {
    minA <- 4 + 1
  } else {
    minA <- 3 + 1
  }
  design <- custom_design(nruns, numcols)
  full_wlp <- wlp(design)
  short_wlp <- full_wlp[minA:(minA + length(numwlp) - 1)]
  expect_equal(short_wlp, numwlp)
})
