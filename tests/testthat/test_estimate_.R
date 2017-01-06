###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for estimate_()")

tmp_date <- 2L

set.seed(123)

n <- 1000L

z  <- rnorm(n)
xz <- rnorm(n)

m_x <- cbind(z, xz)

betas <- c(2, -1)

y <- m_x %*% betas + rnorm(n)

df_data <- data.frame(z  = z,
                      xz = xz,
                      y  = y)

test_that("models with no intercept work", {
  lm_fit <- lm.fit(x = m_x, y = y)

  expect_identical(estimate_(list(x = c("z", "xz"),
                                  y = "y"),
                             .data = df_data,
                             .date = tmp_date),
                   list(date = tmp_date,
                        coefficients = lm_fit$coefficients,
                        r.squared = r_squared_from_lm_fit_(lm_fit)))
})

test_that("models with intercept work", {
  lm_fit <- lm.fit(x = cbind(`(Intercept)` = 1L, m_x), y = y)

  expect_identical(estimate_(list(x = c("z", "xz"),
                                  y = "y",
                                  intercept = TRUE),
                             .data = df_data,
                             .date = tmp_date),
                   list(date = tmp_date,
                        coefficients = lm_fit$coefficients,
                        r.squared = r_squared_from_lm_fit_(lm_fit)))
})
