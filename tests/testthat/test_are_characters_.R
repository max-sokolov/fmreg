###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for are_characters()")

f <- are_characters_

test_that("The output has right type and form", {
  arg1 <- "me"
  arg2 <- c("ret", "bm")
  expect_true(is.logical(f(arg1, arg2)))

  expect_true(length(    f(arg1, arg2)) == 1)
})

test_that("f deals with no args properly", {
  expect_that(f(), throws_error())
})

test_that("f gives right answers in simple cases", {
  # one args
  expect_identical(f("me"), TRUE)
  expect_identical(f(1), FALSE)
  expect_identical(f(1L), FALSE)
  expect_identical(f(TRUE), FALSE)
  expect_identical(f(list()), FALSE)

  # two args
  arg1 <- c("me")

  arg2 <- c("ret", "bm")
  expect_identical(f(arg1, arg2), TRUE)

  arg2 <- 1
  expect_identical(f(arg1, arg2), FALSE)

  arg2 <- list("ret", "bm")
  expect_identical(f(arg1, arg2), FALSE)
})
