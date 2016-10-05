###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Prepare data for the Fama-MacBeth regression.
#'
#' \code{prepare_data_} transforms the data before running
#' the Fama-MacBeth regression.
#'
#' @param .data      Data frame with the data
#' @param y          Name of the dependent variable
#' @param X          Names of the regressors
#' @param date_var   Name of the date variable
#' @param winsorize  TRUE/FALSE
#' @param trim       TRUE/FALSE
#' @param cutoffs    Lower and upper cutoffs for winsorization/trimming
#'
#' @return data frame with the transformed data.

#' @keywords internal
prepare_data_ <- function(.data, y, X, date_var, winsorize, trim, cutoffs){
  # _____________________________ winsorize/trim ______________________________
  if (winsorize == TRUE){
    method <- "winsorize"

    if (trim == TRUE){
      stop("'winsorize' and 'trim' cannot be applied at the same time.")
    }
  } else if (trim == TRUE){
    method <- "trim"
  }

  if (winsorize == TRUE || trim == TRUE){
    # mutate cross-sections for each time period indipendently
    .data <- mutate_cs(.data, vars = X, date_var = date_var,
                       method = method, cutoffs = cutoffs)
  }

  # ________________________________ filter NAs _______________________________
  .data <- tidyr::drop_na_(.data, vars = c(y, X, date_var))

  # __________________________________ return _________________________________
  .data
}
