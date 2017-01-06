###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Estimate the second stage of the Fama-MacBeth regression.
#'
#' \code{aggregate_} aggregates the results from the first stage of
#' the Fama-MacBeth regression.
#'
#' @param model  A list specifying the model.
#'               E.g., x and y fields should contain rhs and lhs of the model.
#' @param l_fit  A list of "fits" from the first stage.
#'               Each "fit" is a list with the result for that "date".
#'
#' @return A list with the results of the Fama-MacBeth regression.
#'         This list is a fmreg object.
#'
#' @seealso \code{\link[fmreg]{estimate_}}

#' @keywords internal
aggregate_ <- function(model, l_fit){
  # take only dates with estimated models
  l_fit <- purrr::discard(l_fit, function(l){length(l) == 0})

  # number of the first state coefficients
  k <- length(rhs_names_(model))

  # extract coefficients into a matrix
  m_coef <- vapply(l_fit,
                   FUN = function(l){l$coefficients},
                   FUN.VALUE = numeric(k))

  if (k == 1){
    # m_coef is not a matrix, but a vector
    df_coef <- dplyr::data_frame(tmp_name = m_coef)
    colnames(df_coef) <- rhs_names_(model)
  } else {
    # don't forget to transpose
    df_coef <- dplyr::as_data_frame(t(m_coef))
  }

  # Fama-MacBeth estimates
  # time series regression for each column
  f_ts_reg <- function(col_name){
    tmp_y <- df_coef[[col_name]]

    tmp_fit <- stats::lm(tmp_y ~ 1)

    df_tmp <- broom::tidy(tmp_fit)

    df_tmp$term <- col_name

    df_tmp
  }

  # time series regressions
  l_ts_est <- lapply(colnames(df_coef), FUN = f_ts_reg)

  # fast rbind to get Fama-MacBeth results
  df_fm <- dplyr::bind_rows(l_ts_est)

  # fmreg's output includes the model
  model_fit <- model

  model_fit$fm_estimates <- df_fm

  model_fit$first_stage_coefficients <- df_coef

  model_fit$dates <- vapply(l_fit,
                            FUN = function(l){l$date},
                            FUN.VALUE = l_fit[[1]]$date)

  model_fit$r.squared <- vapply(l_fit,
                                FUN = function(l){l$r.squared},
                                FUN.VALUE = numeric(1))

  class(model_fit) <- "fmreg"

  model_fit
}
