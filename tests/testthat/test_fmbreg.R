###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for fmbreg()")

f <- fmbreg

data_frame <- dplyr::data_frame
`%>%`      <- dplyr::`%>%`
filter     <- dplyr::filter
select     <- dplyr::select

test_that("f gives right answers in a simple case", {
  n <- 1000L
  set.seed(123)
  x <- rnorm(n)
  y1 <- x + rnorm(n)
  y2 <- x + rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        y    = c(y1, y2),
                        x    = c(x, x))

  fmb_fit <- f(df_data, y = "y", X = "x", date_var = "date")

  expect_true(all(c("fmb_estimates", "cs_estimates") %in% names(fmb_fit)))

  # estimates
  expect_true(all.equal(as.double(
                        fmb_fit$fmb_estimates %>%
                          filter(term == "x") %>%
                          select(estimate)
                        ),
                        1,
                        tolerance = 0.1))

  expect_true(all.equal(as.double(
                        fmb_fit$fmb_estimates %>%
                          filter(term == "(Intercept)") %>%
                          select(estimate)
                        ),
                        0,
                        tolerance = 0.1))

  # t-statistics
  expect_true(abs(fmb_fit$fmb_estimates %>%
                    filter(term == "x") %>%
                    select(statistic)
                  ) > 2.0)

  expect_true(abs(fmb_fit$fmb_estimates %>%
                    filter(term == "(Intercept)") %>%
                    select(statistic)
                  ) < 2.0)
})

test_that("f gives right answers in a more complicated,
           but still simple case", {
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

  fmb_fit <- f(df_data, y = "y", X = c("x", "z"), date_var = "date")

  expect_identical(colnames(fmb_fit$fmb_estimates),
                   c("term", "estimate", "std.error", "statistic", "p.value"))

  expect_identical(fmb_fit$fmb_estimates[["term"]],
                   c("(Intercept)", "x", "z"))

  expect_identical(colnames(fmb_fit$cs_estimates),
                   c("date", "r.squared", "(Intercept)", "x", "z"))

  expect_true(nrow(fmb_fit$cs_estimates) == T)

  # estimates
  expect_true(all.equal(as.double(
                        fmb_fit$fmb_estimates %>%
                          filter(term == "x") %>%
                          select(estimate)
                        ),
                        1,
                        tolerance = 0.1))

  expect_true(all.equal(as.double(
                        fmb_fit$fmb_estimates %>%
                          filter(term == "z") %>%
                          select(estimate)
                        ),
                        -1,
                        tolerance = 0.1))

  expect_true(all.equal(as.double(
                        fmb_fit$fmb_estimates %>%
                          filter(term == "(Intercept)") %>%
                          select(estimate)
                        ),
                        0,
                        tolerance = 0.1))

  # t-statistics
  expect_true(abs(fmb_fit$fmb_estimates %>%
                    filter(term == "x") %>%
                    select(statistic)
                  ) > 2.0)

  expect_true(abs(fmb_fit$fmb_estimates %>%
                    filter(term == "z") %>%
                    select(statistic)
                  ) > 2.0)

  expect_true(abs(fmb_fit$fmb_estimates %>%
                    filter(term == "(Intercept)") %>%
                    select(statistic)
                  ) < 2.0)
})

test_that("f gives a warning if the cross-section is less than 100 obs", {
  n <- 99L
  set.seed(123)
  x <- rnorm(n)
  y1 <- x + rnorm(n)
  y2 <- x + rnorm(n)

  df_data <- data_frame(date = c(rep(1L, times = n), rep(2L, times = n)),
                        y    = c(y1, y2),
                        x    = c(x, x))

  expect_warning(f(df_data, y = "y", X = "x", date_var = "date"))
})
