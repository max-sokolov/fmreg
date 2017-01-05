###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for rhs_names_()")

f <- rhs_names_

test_that("models with no intercept work", {
  x <- c("z")
  expect_identical(f(list(x = x)), x)

  x <- c("z", "xz")
  expect_identical(f(list(x = x)), x)

  expect_error(f(list(y = "y")),
               "The model should contain at least one RHS variable.")
})

test_that("models with intercept work", {
  # illegal value
  expect_error(f(list(intercept = "include")),
               "'intercept' should be TRUE or FALSE")

  # intercept = FALSE
  x <- c("z")
  expect_identical(f(list(x = x, intercept = FALSE)), x)

  x <- c("z", "xz")
  expect_identical(f(list(x = x, intercept = FALSE)), x)

  expect_error(f(list(y = "y", intercept = FALSE)),
               "The model should contain at least one RHS variable.")

  # intercept = TRUE
  x <- c("z")
  expect_identical(f(list(x = x, intercept = TRUE)),
                   c("(Intercept)", x))

  x <- c("z", "xz")
  expect_identical(f(list(x = x, intercept = TRUE)),
                   c("(Intercept)", x))

  expect_error(f(list(x = c("x", "(Intercept)"), intercept = TRUE)))
})
