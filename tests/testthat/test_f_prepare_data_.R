###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for prepare_data_()")

f <- prepare_data_
drop_na_ <- tidyr::drop_na_

test_that("f returns an initial data frame
           when nothing is needed to be done", {
  df_data <- data.frame(i = c(1L, 2L),
                        d = c(1,  2),
                        c = c("a", "ab"),
                        l = c(TRUE, FALSE),
                        stringsAsFactors = FALSE)

  expect_identical(df_data, f(df_data,
                              y = "d", x = c("d", "i"), date_var = "c",
                              winsorize = FALSE, trim = FALSE,
                              cutoffs = c(0.01, 0.99)))
})

test_that("f returns an initial data frame without NAs
           even when nothing is needed to be done", {
  df_data <- data.frame(i = c(1L, 2L, NA),
                        d = c(1,  2,  3),
                        c = c("a", "ab", "c"),
                        l = c(TRUE, FALSE, "TRUE"),
                        stringsAsFactors = FALSE)

  expect_identical(dplyr::filter(df_data, is.na(i) == FALSE),
                   f(df_data,
                     y = "d", x = c("d", "i"), date_var = "c",
                     winsorize = FALSE, trim = FALSE,
                     cutoffs = c(0.01, 0.99)))
})

test_that("f is doing what it is supposed to do", {
  n  <- 1000
  nt <- 100
  cutoffs <- c(0.01, 0.99)

  set.seed(123)

  df_data <- data.frame(y = rnorm(n*nt),
                        x = rnorm(n*nt),
                        z = rnorm(n*nt),
                        date = rep(seq(1, n), times = nt),
                        stringsAsFactors = FALSE)

  # winsorize
  expect_identical(mutate_cs_(df_data, vars = c("x", "z"), date_var = "date",
                              method = "winsorize", cutoffs = cutoffs),
                   f(df_data, y = "y", x = c("x", "z"), date_var = "date",
                     winsorize = TRUE, trim = FALSE,
                     cutoffs = cutoffs))

  # trim
  expect_identical(drop_na_(mutate_cs_(df_data, vars = c("x", "z"), date_var = "date",
                                       method = "trim", cutoffs = cutoffs),
                            vars = c("x", "z")),
                   f(df_data, y = "y", x = c("x", "z"), date_var = "date",
                     winsorize = FALSE, trim = TRUE,
                     cutoffs = cutoffs))

  # raise an error when both winsorize and trim are ON
  expect_error(f(df_data, y = "y", x = c("x", "z"), date_var = "date",
                 winsorize = TRUE, trim = TRUE,
                 cutoffs = cutoffs),
                "'winsorize' and 'trim' cannot be applied at the same time.")
})
