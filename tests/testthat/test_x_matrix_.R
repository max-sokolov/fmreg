###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for x_matrix_()")

df_1 <- data.frame(z = 1, xz = 3)
df_2 <- data.frame(z = c(1, 2), xz = c(3, 4))

test_that("models with no intercept work", {
  x <- c("z")

  expect_identical(x_matrix_(df_1, list(x = x)),
                   as.matrix(df_1[, x, drop = FALSE]))

  expect_identical(x_matrix_(df_2, list(x = x)),
                   as.matrix(df_2[, x, drop = FALSE]))

  x <- c("z", "xz")

  expect_identical(x_matrix_(df_1, list(x = x)),
                   as.matrix(df_1[, x, drop = FALSE]))

  expect_identical(x_matrix_(df_2, list(x = x)),
                   as.matrix(df_2[, x, drop = FALSE]))
})

test_that("models with intercept work", {
  # illegal value
  expect_error(x_matrix_(df_1, list(intercept = "include")),
               "'intercept' should be TRUE or FALSE")

  expect_error(x_matrix_(df_2, list(intercept = "include")),
               "'intercept' should be TRUE or FALSE")

  # intercept = FALSE
  x <- c("z")

  expect_identical(x_matrix_(df_1, list(x = x, intercept = FALSE)),
                   as.matrix(df_1[, x, drop = FALSE]))

  expect_identical(x_matrix_(df_2, list(x = x, intercept = FALSE)),
                   as.matrix(df_2[, x, drop = FALSE]))

  x <- c("z", "xz")

  expect_identical(x_matrix_(df_1, list(x = x, intercept = FALSE)),
                   as.matrix(df_1[, x, drop = FALSE]))

  expect_identical(x_matrix_(df_2, list(x = x, intercept = FALSE)),
                   as.matrix(df_2[, x, drop = FALSE]))

  # intercept = TRUE
  df_1_mod <- df_1
  df_1_mod$`(Intercept)` <- 1L

  df_2_mod <- df_2
  df_2_mod$`(Intercept)` <- 1L

  x <- c("z")

  expect_identical(x_matrix_(df_1, list(x = x, intercept = TRUE)),
                   as.matrix(df_1_mod[, c("(Intercept)", x), drop = FALSE]))

  expect_identical(x_matrix_(df_2, list(x = x, intercept = TRUE)),
                   as.matrix(df_2_mod[, c("(Intercept)", x), drop = FALSE]))

  expect_error(x_matrix_(df_1_mod, list(x = x, intercept = TRUE)))
  expect_error(x_matrix_(df_2_mod, list(x = x, intercept = TRUE)))

  x <- c("z", "xz")

  expect_identical(x_matrix_(df_1, list(x = x, intercept = TRUE)),
                   as.matrix(df_1_mod[, c("(Intercept)", x), drop = FALSE]))

  expect_identical(x_matrix_(df_2, list(x = x, intercept = TRUE)),
                   as.matrix(df_2_mod[, c("(Intercept)", x), drop = FALSE]))

  expect_error(x_matrix_(df_1_mod, list(x = x, intercept = TRUE)))
  expect_error(x_matrix_(df_2_mod, list(x = x, intercept = TRUE)))
})
