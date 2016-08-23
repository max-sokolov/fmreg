###############################################################################
# (c) Maxim Sokolov
###############################################################################

f <- r_squared_from_lm_fit

# _____________________________________________________________________________
context("Main check for r_squared_from_lm_fit()")

test_that("the f gives right answers in simple cases", {
  set.seed(123)
  
  n <- 1000
  x_1 <- rnorm(n)
  x_2 <- runif(n)
  x_3 <- rnorm(n) + 1
  x_4 <- rnorm(n) + 2

  y <- 1 + x_1 + 2*x_2 + 0.5*x_4 + 2*rnorm(n)

  # _____________________________ full model __________________________________
  X <- cbind(1, x_1, x_2, x_3, x_4)

  res_lm_fit <- lm.fit(x = X, y = y)

  res_lm     <- lm(y ~ x_1 + x_2 + x_3 + x_4)

  # R^2
  expect_equal(f(res_lm_fit), summary(res_lm)$r.squared)

  # R^2 adjusted
  expect_equal(f(res_lm_fit, is_adj = TRUE), summary(res_lm)$adj.r.squared)

  # _____________________________ small model __________________________________
  x <- cbind(1, x_1)

  res_lm_fit <- lm.fit(x = x, y = y)

  res_lm     <- lm(y ~ x_1)

  # R^2
  expect_equal(f(res_lm_fit), summary(res_lm)$r.squared)

  # R^2 adjusted
  expect_equal(f(res_lm_fit, is_adj = TRUE), summary(res_lm)$adj.r.squared)

  # _____________________ small model: no intercept ___________________________
  x_no <- cbind(x_1)

  res_lm_fit <- lm.fit(x = x_no, y = y)

  res_lm     <- lm(y ~ 0 + x_1)

  # R^2
  expect_equal(f(res_lm_fit, intercept = FALSE), summary(res_lm)$r.squared)

  # R^2 adjusted
  expect_equal(f(res_lm_fit, intercept = FALSE, is_adj = TRUE),
               summary(res_lm)$adj.r.squared)
})
