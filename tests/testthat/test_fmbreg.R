###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for fmbreg()")

f <- fmbreg

data_frame <- dplyr::data_frame

test_that("f gives right answers in a simple case", {
  n <- 1000L
  set.seed(20151128)
  x <- rnorm(n)
  y1 <- x + rnorm(n)
  y2 <- x + rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        y    = c(y1, y2),
                        x    = c(x, x))

  res <- f(df_data, y = "y", X = "x", date_var = "date")

  expect_true(all(c("est", "t_stat", "full_est") %in% names(res)))

  expect_true(all.equal(res$est[["x"]],     1, tolerance = 0.1))
  expect_true(all.equal(res$est[["const"]], 0, tolerance = 0.1))

  expect_true(abs(res$t_stat[["x"]]) > 2.0)
  expect_true(abs(res$t_stat[["const"]]) < 2.0)  
})

test_that("f gives right answers in a more complicated simple case", {
  n <- 1000L
  T <- 100L
  
  set.seed(20151128)
  x <- rnorm(n)
  z <- rnorm(n)

  v_colnames <- c("date", "y", "x", "z")
  df_data <- as.data.frame(matrix(NA, nrow = n*T, ncol = length(v_colnames)))
  colnames(df_data) <- v_colnames

  df_data$x <- rep(x, times = T)
  df_data$z <- rep(z, times = T)

  for (tmp_t in seq(1, T)){
    tmp_y <- x * (1  + 0.1*rnorm(n)) + 
             z * (-1 + 0.1*rnorm(n)) + 
             3*rnorm(n)

    tmp_ind <- seq((tmp_t-1)*n + 1, tmp_t*n)

    df_data[tmp_ind, "date"] <- tmp_t
    df_data[tmp_ind, "y"] <- tmp_y
  }

  res <- f(df_data, y = "y", X = c("x", "z"), date_var = "date")

  expect_identical(names(res$est),    c("const", c("x", "z")))
  expect_identical(names(res$t_stat), c("const", c("x", "z")))
  expect_identical(colnames(res$full_est), c("date", "R2", "const", c("x", "z")))

  expect_true(nrow(res$full_est) == T)

  expect_true(all.equal(res$est[["x"]],     1, tolerance = 0.1))
  expect_true(all.equal(res$est[["z"]],    -1, tolerance = 0.1))
  expect_true(all.equal(res$est[["const"]], 0, tolerance = 0.1))

  expect_true(abs(res$t_stat[["x"]]) > 2.0)
  expect_true(abs(res$t_stat[["z"]]) > 2.0)
  expect_true(abs(res$t_stat[["const"]]) < 2.0)  
})

test_that("f gives a warning if the cross-section is less than 100 obs", {
  n <- 99L
  set.seed(20151128)
  x <- rnorm(n)
  y1 <- x + rnorm(n)
  y2 <- x + rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        y    = c(y1, y2),
                        x    = c(x, x))

  expect_warning(f(df_data, y = "y", X = "x", date_var = "date"))
})
