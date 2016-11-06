###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for require_length_()")

f <- require_length_

test_that("the function works", {
  arg1 <- "me"
  arg2 <- c("ret", "bm")

  expect_true(f(arg1, 1))
  expect_true(f(arg2, 2))

  expect_error(f(arg1, 3), "Variable arg1 is supposed to be of length 3.")
  expect_error(f(arg2, 1), "Variable arg2 is supposed to be of length 1.")
})
