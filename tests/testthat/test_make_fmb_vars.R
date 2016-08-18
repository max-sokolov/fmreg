###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for make_fmb_vars()")

f <- make_fmb_vars

data_frame <- dplyr::data_frame

trim <- portfs::trim
winsorize <- portfs::winsorize

test_that("f gives right answers in the simplest case", {
  # 100 firms, 2 periods
  n <- 100
  v_dates <- c(20151031L, 20151130L)
  v_id    <- seq(1, n)
  v_rand  <- rnorm(n)

  cutoffs <- c(0.10, 0.90)

  df_data <- data_frame(permno = rep(v_id, times = 2),
                        date   = c(rep(v_dates[1], times = n),
                                   rep(v_dates[2], times = n)),
                        a      = c(v_rand, v_rand))

  # winsorize
  df_res  <- data_frame(permno = v_id,
                        date   = rep(v_dates[2], times = n),
                        a      = v_rand,
                        fmb_a  = winsorize(v_rand, cutoffs = cutoffs))

  expect_identical(as.list(f(df_data, vars = "a", date_var = "date", id_var = "permno",
                             cutoffs = cutoffs, method = "winsorize")),
                   as.list(df_res))

  # trim
  df_res  <- data_frame(permno = v_id,
                        date   = rep(v_dates[2], times = n),
                        a      = v_rand,
                        fmb_a  = trim(v_rand, cutoffs = cutoffs))

  df_res <- dplyr::filter(df_res, is.na(fmb_a) == FALSE)

  expect_identical(as.list(f(df_data, vars = "a", date_var = "date", id_var = "permno",
                             cutoffs = cutoffs, method = "trim")),
                   as.list(df_res))  
})

test_that("f gives right answers in a bit more complicated case", {
  # 100 firms, 3 periods, 2 vars
  n <- 100
  v_dates <- c(20150930L, 20151031L, 20151130L)
  v_id    <- seq(1, n)
  v_rand  <- rnorm(n)

  cutoffs <- c(0.10, 0.90)

  df_data <- data_frame(permno = c(v_id, v_id, v_id),
                        date   = c(rep(v_dates[1], times = n),
                                   rep(v_dates[2], times = n),
                                   rep(v_dates[3], times = n)),
                        a      = c(v_rand, v_rand + 1, v_rand + 2),
                        b      = c(v_id, v_id, v_id))

  # winsorize
  df_res  <- data_frame(permno = c(v_id, v_id),
                        date   = c(rep(v_dates[2], times = n),
                                   rep(v_dates[3], times = n)),
                        a      = c(v_rand + 1, v_rand + 2),
                        b      = c(v_id, v_id),
                        fmb_a  = c(winsorize(v_rand,     cutoffs = cutoffs),
                                   winsorize(v_rand + 1, cutoffs = cutoffs)),
                        fmb_b  = c(winsorize(v_id, cutoffs = cutoffs),
                                   winsorize(v_id, cutoffs = cutoffs)))

  df_res <- dplyr::arrange(df_res, permno, date)

  expect_identical(as.list(f(df_data, vars = c("a", "b"),
                             date_var = "date", id_var = "permno",
                             cutoffs = cutoffs, method = "winsorize")),
                   as.list(df_res))

  # trim
  df_res  <- data_frame(permno = c(v_id, v_id),
                        date   = c(rep(v_dates[2], times = n),
                                   rep(v_dates[3], times = n)),
                        a      = c(v_rand + 1, v_rand + 2),
                        b      = c(v_id, v_id),
                        fmb_a  = c(trim(v_rand,     cutoffs = cutoffs),
                                   trim(v_rand + 1, cutoffs = cutoffs)),
                        fmb_b  = c(trim(v_id, cutoffs = cutoffs),
                                   trim(v_id, cutoffs = cutoffs)))

  df_res <- dplyr::filter(df_res, is.na(fmb_a) == FALSE, is.na(fmb_b) == FALSE)

  df_res <- dplyr::arrange(df_res, permno, date)

  expect_identical(as.list(f(df_data, vars = c("a", "b"),
                             date_var = "date", id_var = "permno",
                             cutoffs = cutoffs, method = "trim")),
                   as.list(df_res))
})