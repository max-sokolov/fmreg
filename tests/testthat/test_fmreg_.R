###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for fmreg_()")

f <- fmreg_

data_frame <- dplyr::data_frame
`%>%`      <- dplyr::`%>%`
filter     <- dplyr::filter
select     <- dplyr::select

test_that("f gives right answers in a two-period case", {
  n <- 1000L
  set.seed(123)
  x <- rnorm(n)
  y1 <- x + rnorm(n)
  y2 <- x + rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        y    = c(y1, y2),
                        x    = c(x, x))

  fm_fit <- f(df_data, y = "y", x = "x", date_var = "date")

  expect_true(all(c("fm_estimates", "cs_estimates") %in% names(fm_fit)))

  # estimates
  expect_true(all.equal(as.double(
                        fm_fit$fm_estimates %>%
                          filter(term == "x") %>%
                          select(estimate)
                        ),
                        1,
                        tolerance = 0.1))

  expect_true(all.equal(as.double(
                        fm_fit$fm_estimates %>%
                          filter(term == "(Intercept)") %>%
                          select(estimate)
                        ),
                        0,
                        tolerance = 0.1))

  # t-statistics
  expect_true(abs(fm_fit$fm_estimates %>%
                    filter(term == "x") %>%
                    select(statistic)
                  ) > 2.0)

  expect_true(abs(fm_fit$fm_estimates %>%
                    filter(term == "(Intercept)") %>%
                    select(statistic)
                  ) < 2.0)
})

test_that("f gives right answers in a multiperiod case", {
  n <- 1000L
  T <- 100L
  
  set.seed(123)
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

  fm_fit <- f(df_data, y = "y", x = c("x", "z"), date_var = "date")

  expect_identical(colnames(fm_fit$fm_estimates),
                   c("term", "estimate", "std.error", "statistic", "p.value"))

  expect_identical(fm_fit$fm_estimates[["term"]],
                   c("(Intercept)", "x", "z"))

  expect_identical(colnames(fm_fit$cs_estimates),
                   c("date", "r.squared", "(Intercept)", "x", "z"))

  expect_true(nrow(fm_fit$cs_estimates) == T)

  # estimates
  expect_true(all.equal(as.double(
                        fm_fit$fm_estimates %>%
                          filter(term == "x") %>%
                          select(estimate)
                        ),
                        1,
                        tolerance = 0.1))

  expect_true(all.equal(as.double(
                        fm_fit$fm_estimates %>%
                          filter(term == "z") %>%
                          select(estimate)
                        ),
                        -1,
                        tolerance = 0.1))

  expect_true(all.equal(as.double(
                        fm_fit$fm_estimates %>%
                          filter(term == "(Intercept)") %>%
                          select(estimate)
                        ),
                        0,
                        tolerance = 0.1))

  # t-statistics
  expect_true(abs(fm_fit$fm_estimates %>%
                    filter(term == "x") %>%
                    select(statistic)
                  ) > 2.0)

  expect_true(abs(fm_fit$fm_estimates %>%
                    filter(term == "z") %>%
                    select(statistic)
                  ) > 2.0)

  expect_true(abs(fm_fit$fm_estimates %>%
                    filter(term == "(Intercept)") %>%
                    select(statistic)
                  ) < 2.0)
})

test_that("f gives a warning if the cross-section is less than 'min_obs' obs", {
  n <- 89L
  set.seed(123)
  x <- rnorm(n)
  y1 <- x + rnorm(n)
  y2 <- x + rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        y    = c(y1, y2),
                        x    = c(x, x))

  expect_silent(f(df_data, y = "y", x = "x", date_var = "date", min_obs = 89))
  expect_warning(f(df_data, y = "y", x = "x", date_var = "date", min_obs = 90),
                 "Date [0-9]+ contains less than [0-9]+ observations")
})

context("Check winsorize/trim arguments in fmreg_()")

f <- fmreg_

data_frame <- dplyr::data_frame

test_that("f gives right answers when winsorize/trim is TRUE", {
  n <- 1000L
  T <- 100L
  
  set.seed(123)
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

  cutoffs = c(0.10, 0.90)

  df_data_win <- mutate_cs(df_data, vars = c("x", "z"), date_var = "date",
                           method = "winsorize", cutoffs = cutoffs)

  df_data_trim <- mutate_cs(df_data, vars = c("x", "z"), date_var = "date",
                            method = "trim", cutoffs = cutoffs)

  fm_fit <- f(df_data, y = "y", x = c("x", "z"), date_var = "date")

  # winsorize
  expect_identical(f(df_data, y = "y", x = c("x", "z"), date_var = "date",
                     winsorize = TRUE, cutoffs = cutoffs),
                   f(df_data_win, y = "y", x = c("x", "z"), date_var = "date"))

  # trim
  expect_identical(f(df_data, y = "y", x = c("x", "z"), date_var = "date",
                     trim = TRUE, cutoffs = cutoffs),
                   f(df_data_trim, y = "y", x = c("x", "z"), date_var = "date"))

  # winsorize + trim should give an error
  expect_error(f(df_data, y = "y", x = c("x", "z"), date_var = "date",
                 winsorize = TRUE, trim = TRUE),
                 "'winsorize' and 'trim' cannot be applied at the same time.")
})

context("Check intercept argument in fmreg_()")

f <- fmreg_

data_frame <- dplyr::data_frame
`%>%`      <- dplyr::`%>%`
filter     <- dplyr::filter
select     <- dplyr::select
mutate     <- dplyr::mutate

test_that("f gives right answers when intercept is ON", {
  n <- 1000L
  T <- 100L
  
  set.seed(123)
  x <- rnorm(n)
  z <- rnorm(n)

  v_colnames <- c("date", "y", "x", "z")
  df_data <- as.data.frame(matrix(NA, nrow = n*T, ncol = length(v_colnames)))
  colnames(df_data) <- v_colnames

  df_data$x     <- rep(x, times = T)
  df_data$z     <- rep(z, times = T)
  df_data$const <- rep(1L, times = n*T)

  for (tmp_t in seq(1, T)){
    tmp_y <- 1 + x * (1  + 0.1*rnorm(n)) +
                 z * (-1 + 0.1*rnorm(n)) +
                 3*rnorm(n)

    tmp_ind <- seq((tmp_t-1)*n + 1, tmp_t*n)

    df_data[tmp_ind, "date"] <- tmp_t
    df_data[tmp_ind, "y"] <- tmp_y
  }

  fm_fit           <- f(df_data, y = "y", x = c("x", "z"), date_var = "date")

  fm_fit_explicit  <- f(df_data, y = "y", x = c("x", "z", "const"), date_var = "date",
                         intercept = FALSE)

  fm_fit_intercept <- f(df_data, y = "y", x = c("x", "z"), date_var = "date",
                         intercept = 0.01)

  # intercept = TRUE and an explicit column with ones: same result
  expect_equal(fm_fit$fm_estimates %>%
                 filter(term == "(Intercept)") %>%
                 select(estimate),
               fm_fit_explicit$fm_estimates %>%
                  filter(term == "const") %>%
                  select(estimate))

  # If intercept = number, then scaling
  expect_equal(fm_fit$fm_estimates %>%
                 filter(term == "(Intercept)") %>%
                 select(estimate),
               fm_fit_intercept$fm_estimates %>%
                  filter(term == "(Intercept)") %>%
                  select(estimate) %>%
                  mutate(estimate = 0.01*estimate))

  # cross-sectional estimates
  expect_equal(fm_fit$cs_estimates,
               fm_fit_intercept$cs_estimates %>%
                  mutate(`(Intercept)` = 0.01*`(Intercept)`))
})
