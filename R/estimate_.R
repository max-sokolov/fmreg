###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Estimate a model on the first stage of the Fama-MacBeth regression.
#'
#' \code{estimate_} estimates a specified model for a "slice" (cross-section)
#' of the data.
#'
#' @param model  A list specifying the model.
#'               E.g., x and y fields should contain rhs and lhs of the model.
#' @param .data  A data frame: the data on which the model is estimated.
#'               These data is assumed to be a "slice" of the full data for
#'               date \code{.date}.
#' @param .date  Date corresponding to the "slice" of the data.
#'
#' @return A list with the fit of the model.
#'         The fit does not include the data itself
#'         because the function is intended to work with relatively big data.
#'         \code{aggregate_} should be able to work with this list on
#'         the second stage of the Fama-MacBeth regression.
#'
#' @seealso \code{\link[fmreg]{aggregate_}}

#' @keywords internal
estimate_ <- function(model, .data, .date){
  if (nrow(.data) == 0){
    return(list())
  }

  model_fit <- stats::lm.fit(x = x_matrix_(.data, model = model),
                             y = .data[[lhs_name_(model)]])

  fit <- list(date = .date,
              coefficients = model_fit$coefficients,
              r.squared = r_squared_from_lm_fit_(model_fit))

  fit
}
