###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for fmreg_()")

f <- fmreg_

data_frame <- dplyr::data_frame

test_that("f gives right answers in a two-period case", {
  n <- 1000L
  set.seed(123)
  x <- rnorm(n)
  y1 <- x + rnorm(n)
  y2 <- x + rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        y    = c(y1, y2),
                        x    = c(x, x))

  l_fmreg <- f(df_data,
               models = list(list(y = "y", x = "x"),
                             list(y = "y", x = "x", intercept = TRUE)),
               dates = c(1L, 2L),
               date_var = "date")

  obj_no   <- l_fmreg[[1]]
  obj_with <- l_fmreg[[2]]

  # structure of the object
  names_fields <- c("fm_estimates", "first_stage_coefficients",
                    "dates", "r.squared")
  expect_true(all(c("y", "x", names_fields) %in% names(obj_no)))
  expect_true(all(c("y", "x", "intercept", names_fields) %in% names(obj_with)))

  # estimates
  expect_true(all.equal(coef(obj_no),   c(x = 1),                    tolerance = 0.1))
  expect_true(all.equal(coef(obj_with), c(`(Intercept)` = 0, x = 1), tolerance = 0.1))
})

test_that("f gives right answers in a multiperiod case", {
  n <- 1000L
  n_periods <- 100L
  
  set.seed(123)
  x <- rnorm(n)
  z <- rnorm(n)

  v_colnames <- c("date", "y", "x", "z")
  df_data <- as.data.frame(matrix(NA, nrow = n*n_periods, ncol = length(v_colnames)))
  colnames(df_data) <- v_colnames

  df_data$x <- rep(x, times = n_periods)
  df_data$z <- rep(z, times = n_periods)

  beta_mean <- c(x = 1,   z = -1)
  beta_sd   <- c(x = 0.1, z = 0.4)
  sd_eps <- 3

  beta <- cbind(x = beta_mean["x"] + beta_sd["x"]*rnorm(n_periods),
                z = beta_mean["z"] + beta_sd["z"]*rnorm(n_periods))

  for (tmp_t in seq(1, n_periods)){
    tmp_y <- x * beta[tmp_t, "x"] + 
             z * beta[tmp_t, "z"] + 
             sd_eps*rnorm(n)

    tmp_ind <- seq((tmp_t-1)*n + 1, tmp_t*n)

    df_data[tmp_ind, "date"] <- tmp_t
    df_data[tmp_ind, "y"] <- tmp_y
  }

  l_fmreg <- f(df_data,
               models = list(list(y = "y", x = c("x", "z")),
                             list(y = "y", x = c("x", "z"), intercept = TRUE)),
               dates = seq(1, n_periods),
               date_var = "date")

  obj_no   <- l_fmreg[[1]]
  obj_with <- l_fmreg[[2]]

  # structure of the object
  names_fields <- c("fm_estimates", "first_stage_coefficients",
                    "dates", "r.squared")
  expect_true(all(c("y", "x", names_fields) %in% names(obj_no)))
  expect_true(all(c("y", "x", "intercept", names_fields) %in% names(obj_with)))

  # estimates
  # no intercept
  expect_equal(coef(obj_no)["x"],
               beta_mean["x"],
               tolerance = 2*2*beta_sd["x"]/sqrt(n_periods)) # second 2 is heuristic

  expect_equal(coef(obj_no)["z"],
             beta_mean["z"],
             tolerance = 2*2*beta_sd["z"]/sqrt(n_periods)) # second 2 is heuristic

  # with intercept
  expect_equal(coef(obj_with)["x"],
               beta_mean["x"],
               tolerance = 2*2*beta_sd["x"]/sqrt(n_periods)) # second 2 is heuristic

  expect_equal(coef(obj_with)["z"],
             beta_mean["z"],
             tolerance = 2*2*beta_sd["z"]/sqrt(n_periods)) # second 2 is heuristic

})
