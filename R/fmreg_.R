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

  # _______________________ cross-sectional regressions _______________________
  df_cs_est <- do_cs_regressions_(.data, y = y, x = x, date_var = date_var,
                                  intercept = intercept, min_obs = min_obs)

  # _________________________ time series regressions _________________________
  df_fm_est <- do_ts_regressions_(df_cs_est,
                                  coef_names = x_names_(x, intercept))

  # __________________________________ Return _________________________________
  l <- list(fm_estimates = df_fm_est,
            cs_estimates = df_cs_est,
            y = y,
            x = x,
            date_var = date_var,
            intercept = intercept)

  class(l) <- "fmreg"

  l
}
