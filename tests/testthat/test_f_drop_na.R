###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for drop_na()")

f <- drop_na

data_frame <- dplyr::data_frame

test_that("drop_vars vector is checked for the type", {
  df_tmp <- data_frame(a = 1)

  expect_that(f(df_tmp, list("a")), throws_error())
  expect_that(f(df_tmp, 1), throws_error())
  expect_that(f(df_tmp, 1L), throws_error())
  expect_that(f(df_tmp, TRUE), throws_error())
})

test_that("f preserves the type of cols of the data.frame", {
  df_tmp <- data_frame(a = c(TRUE, FALSE),
                       b = c(1L,   2L),
                       c = c(1,    2),
                       d = c("a",  "b"))

  expect_identical(f(df_tmp, filter_vars = c("a", "b", "c", "d")),
                   df_tmp)
})

test_that("f throws an error if a name is not a colname", {
  expect_that(f(df_tmp, filter_vars = c("b", "zyx")), throws_error())
})

test_that("f throws an error if no drop_vars", {
  expect_that(f(df_tmp, filter_vars = c()), throws_error())
})

test_that("f gives right answers in simple cases", {
  df_tmp <- data_frame(a = c(TRUE, NA),
                       b = c(1L,   2L),
                       c = c(1,    2),
                       d = c("a",  "b"))

  df_expected <- data_frame(a = TRUE,
                            b = 1L,
                            c = 1,
                            d = "a")

  expect_identical(f(df_tmp, filter_vars = "a"),
                   df_expected)

  expect_identical(f(df_tmp, filter_vars = c("a", "b")),
                   df_expected)
})

test_that("f works in case of only one column", {
  df_tmp <- data_frame(a = c(1, NA))
  df_expected <- data_frame(a = 1)

  expect_identical(f(df_tmp, filter_vars = "a"),
                   df_expected)
})
