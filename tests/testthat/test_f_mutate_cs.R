###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for mutate_cs()")

f <- mutate_cs

data_frame <- dplyr::data_frame

test_that("f gives right answers in the simplest case", {
  # 1000 firms, 1 period
  n    <- 1000

  df_data <- data_frame(date = rep(1L, times = n),
                        a    = seq(1, n))

  cutoffs = c(0.01, 0.99)

  # winsorization
  df_res <- dplyr::mutate(df_data, a = winsorize(a, cutoffs))

  expect_identical(f(df_data, vars = "a", date_var = "date",
                     method = "winsorize", cutoffs = cutoffs),
                   df_res)

  # trimming
  df_res <- dplyr::mutate(df_data, a = trim(a, cutoffs))

  expect_identical(f(df_data, vars = "a", date_var = "date",
                     method = "trim", cutoffs = cutoffs),
                   df_res)
})

test_that("f gives right answers in a bit complicated case", {
  # 1000 firms, 2 period
  n    <- 1000

  v_rand <- rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        a    = c(seq(1, n),          2*seq(1, n)),
                        b    = c(v_rand,             v_rand))

  cutoffs = c(0.01, 0.99)

  # winsorization
  df_res <- dplyr::mutate(df_data,
                          a = c(winsorize(seq(1, n),   cutoffs),
                                winsorize(2*seq(1, n), cutoffs)),
                          b = c(winsorize(v_rand, cutoffs),
                                winsorize(v_rand, cutoffs))
                         )

  expect_identical(f(df_data, vars = c("a", "b"), date_var = "date",
                     method = "winsorize", cutoffs = cutoffs),
                   df_res)

  # trimming
  df_res <- dplyr::mutate(df_data,
                          a = c(trim(seq(1, n),   cutoffs),
                                trim(2*seq(1, n), cutoffs)),
                          b = c(trim(v_rand, cutoffs),
                                trim(v_rand, cutoffs))
                         )

  expect_identical(f(df_data, vars = c("a", "b"), date_var = "date",
                     method = "trim", cutoffs = cutoffs),
                   df_res)
})
