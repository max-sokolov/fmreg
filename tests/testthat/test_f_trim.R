###############################################################################
# (c) Maxim Sokolov
###############################################################################

f <- trim

# _____________________________________________________________________________
context("Main check for trim()")

test_that("the inputs are checked", {
  expect_that(f(c(1, 2), cutoffs = c(0.01, 0.50, 0.99)), throws_error())
  expect_that(f(c(1, 2), cutoffs = c(0.00, 1.01)),       throws_error())
  expect_that(f(c(1, 2), cutoffs = c(0.99, 0.01)),       throws_error())
})

test_that("the f gives right answers in simple cases", {
  x <- seq(1, 1000)
  cutoffs <- c(0.01, 0.99)

  x_trim <- x
  x_trim[x_trim < 10] <- NA
  x_trim[x_trim > 990] <- NA

  expect_identical(f(x, cutoffs = cutoffs), x_trim)

  # reshuffle
  index <- sample(x, size = length(x), replace = FALSE)

  expect_identical(f(x[index], cutoffs = cutoffs), x_trim[index])

  # add NAs - the result is the same
  v_na <- rep(NA, times = 1000)

  expect_identical(f(c(x, v_na), cutoffs = cutoffs), c(x_trim, v_na))
})
