###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Prepare data for the Fama-MacBeth regression.
#'
#' \code{prepare_data_} can be used to make common transformations the data
#' before running the Fama-MacBeth regression.
#'
#' @param .data      Data frame with the data
#' @param y          Name of the dependent variable
#' @param x          Names of the regressors
#' @param date_var   Name of the date variable
#' @param winsorize  TRUE/FALSE
#' @param trim       TRUE/FALSE
#' @param cutoffs    Lower and upper cutoffs for winsorization/trimming
#'
#' @return data frame with the transformed data.

#' @export
prepare_data_ <- function(.data, y, x, date_var, winsorize, trim, cutoffs){
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
  require_length_(winsorize, 1)
  require_length_(trim, 1)
  require_length_(cutoffs, 2)

  if (all(cutoffs >= 0 & cutoffs <= 1) == FALSE){
    stop("Cutoffs should be between 0 and 1.")
  }

  if (cutoffs[1] > cutoffs[2]){
    stop("cutoffs[1] should be less or equal to cutoffs[2].")
  }

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
    # mutate cross-sections for each time period independently
    .data <- mutate_cs_(.data, vars = x, date_var = date_var,
                        method = method, cutoffs = cutoffs)
  }

  # ________________________________ filter NAs _______________________________
  .data <- tidyr::drop_na_(.data, vars = c(y, x, date_var))

  # __________________________________ return _________________________________
  .data
}
