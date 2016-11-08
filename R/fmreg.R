###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Fama-MacBeth regression.
#'
#' \code{fmreg} estimates Fama-MacBeth regression.
#'
#' @param .data     Data frame with the data
#' @param y         Name of the dependent variable
#'                  (should be a colname in .data)
#' @param x         Names of the regressors
#'                  (should be colnames in .data)
#' @param date_var  Name of the date variable
#'                  (should be a colname in .data)
#' @param intercept Logical/Double: If FALSE, no intercept.
#'                                  If TRUE, add intercept.
#'                                  If intercept = a,
#'                                  the constant regressor is of form
#'                                  rep(a, times = N) instead of 
#'                                  rep(1L, times = N).
#' @param winsorize Logical: If TRUE, winsorize the regressors.
#' @param trim      Logical: If TRUE, trim the regressors.
#' @param cutoffs   Vector with two numbers between 0 and 1:
#'                  lower and upper cutoffs for winsorization/trimming.
#' @param min_obs   Number: If a cross-section contains less than
#'                          \code{min_obs} observations, a warning is issued. 
#'
#' @return A list: $fm_estimates - data frame with Fama-MacBeth estimates;
#'                 $cs_estimates  - data frame with cross-sectional estimates
#'                                  for every period.

#' @export
fmreg <- function(.data, y, x, date_var, intercept = TRUE,
                   winsorize = FALSE, trim = FALSE, cutoffs = c(0.01, 0.99),
                   min_obs = 100){

  # _______________________________ prepare data ______________________________
  .data <- prepare_data_(.data, y = y, x = x, date_var = date_var,
                         winsorize = winsorize, trim = trim, cutoffs = cutoffs)

  # __________________________ Fama-MacBeth regression ________________________
  fmreg_(.data, y = y, x = x, date_var = date_var, intercept = intercept,
         min_obs = min_obs)
}
