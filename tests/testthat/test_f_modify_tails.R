###############################################################################
# (c) Maxim Sokolov
###############################################################################

f <- modify_tails

# _____________________________________________________________________________
context("Main check for modify_tails()")

test_that("the inputs are checked", {
  expect_that(f(c(1, 2), cutoffs = c(0.01, 0.50, 0.99)), throws_error())
  expect_that(f(c(1, 2), cutoffs = c(0.00, 1.01)),       throws_error())
  expect_that(f(c(1, 2), cutoffs = c(0.99, 0.01)),       throws_error())

  expect_that(f(c(1, 2), cutoffs = c(0.01, 0.50, 0.99), method = "trim"), throws_error())
  expect_that(f(c(1, 2), cutoffs = c(0.00, 1.01), method = "trim"),       throws_error())
  expect_that(f(c(1, 2), cutoffs = c(0.99, 0.01), method = "trim"),       throws_error())

  expect_that(f(c(1, 2), cutoffs = c(0.1, 0.9), method = "some"), throws_error())
})

test_that("the f gives right answers in simple cases", {
  x <- seq(1, 1000)
  cutoffs <- c(0.01, 0.99)

  x_win <- x
  x_win[x_win < 10] <- 10L
  x_win[x_win > 990] <- 990L

  x_trim <- x
  x_trim[x_trim < 10] <- NA
  x_trim[x_trim > 990] <- NA

  expect_identical(f(x, cutoffs = cutoffs), x_win)
  expect_identical(f(x, cutoffs = cutoffs, method = "trim"), x_trim)

  # reshuffle
  index <- sample(x, size = length(x), replace = FALSE)

  expect_identical(f(x[index], cutoffs = cutoffs), x_win[index])
  expect_identical(f(x[index], cutoffs = cutoffs, method = "trim"), x_trim[index])

  # add NAs - the result is the same
  v_na <- rep(NA, times = 1000)

  expect_identical(f(c(x, v_na), cutoffs = cutoffs), c(x_win, v_na))
  expect_identical(f(c(x, v_na), cutoffs = cutoffs, method = "trim"), c(x_trim, v_na))
})
