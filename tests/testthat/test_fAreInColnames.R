###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for AreInColnames()")

f <- AreInColnames

test_that("f deals with no args properly", {
  expect_that(f(), throws_error())
  expect_that(f(data.frame(a = 1)),
              throws_error("provide at least one colname"))
})

test_that("f gives right answers in simple cases", {
  df_data <- data.frame(a = c(1, 2), b = c(3, 4))

  # one arg
  expect_identical(f(df_data, "a"),         TRUE)
  expect_identical(f(df_data, c("a", "a")), TRUE)
  expect_identical(f(df_data, c("a", "b")), TRUE)
  expect_identical(f(df_data, c("a", "D")), FALSE)

  # two args
  expect_identical(f(df_data, c("a", "a"), "b"),        TRUE)
  expect_identical(f(df_data, c("a", "a"), c("b", "a")), TRUE)
  expect_identical(f(df_data, c("a", "a"), c("b", "J")), FALSE)
})
