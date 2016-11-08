###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Fama-MacBeth regression.
#'
#' \code{fmreg_} estimates Fama-MacBeth regression.
#'
#' @param .data     Data frame with the data
#' @param y         Name of the dependent variable
#'                  (should be a colname in .data)
#' @param x         Names of the regressors
#'                  (should be colnames in .data)
#' @param date_var  Name of the date variable
#'                  (should be a colname in .data)
#' @param intercept Logical: If FALSE, no intercept.
#'                           If TRUE, add intercept.
#' @param min_obs   Number: If a cross-section contains less than
#'                          \code{min_obs} observations, a warning is issued.
#'
#' @return A list: $fm_estimates - data frame with Fama-MacBeth estimates;
#'                 $cs_estimates  - data frame with cross-sectional estimates
#'                                  for every period.
#'
#' @seealso \code{\link[fmreg]{fmreg}}

#' @export
fmreg_ <- function(.data, y, x, date_var, intercept = TRUE, min_obs = 100){

  # ____________________________ check arguments ______________________________
  if (are_characters_(y, x, date_var) == FALSE){
    stop("Arguments y, X, and date_var need to be character vectors.")
  }

  if (all(c(y, x, date_var) %in% colnames(.data)) == FALSE){
    stop("Names in y, X, and date_var need to be names from .data colnames.")
  }

  # check the length of the args
  require_length_(y, 1)
  require_length_(date_var, 1)
  require_length_(intercept, 1)

  # ____________________________ augment regressors ___________________________
  if (intercept == FALSE){
    x_aug <- x
  } else {
    if ("(Intercept)" %in% colnames(.data)){
      stop("If you want to include intercept in the model,
            .data should not contain a column named '(Intercept)'.")
    }

    if (intercept == TRUE){
      .data$`(Intercept)` <- 1L
    } else {
      stop("'intercept' should be TRUE or FALSE")
    }

    x_aug <- c("(Intercept)", x)
  }

  n_regressors <- length(x_aug)

  # _______________________ cross-sectional regressions _______________________
  df_cs_est <- do_cs_regressions_(.data, y = y, x = x_aug, date_var = date_var,
                                  min_obs = min_obs)

  # __________________________ Fama-MacBeth estimates _________________________
  for (j in seq_along(x_aug)){
    tmp_name <- x_aug[j]

    tmp_y <- df_cs_est[, tmp_name, drop = TRUE]

    tmp_fit <- stats::lm(tmp_y ~ 1)

    df_tmp <- broom::tidy(tmp_fit)

    df_tmp$term <- tmp_name

    if (j == 1){
      df_fm_est <- df_tmp
    } else {
      df_fm_est <- rbind(df_fm_est, df_tmp)
    }
  }

  # __________________________________ Return _________________________________
  list(fm_estimates = df_fm_est,
       cs_estimates  = df_cs_est)
}
