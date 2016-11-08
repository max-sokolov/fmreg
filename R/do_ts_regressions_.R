###############################################################################
# (c) Maxim Sokolov
###############################################################################

do_ts_regressions_ <- function(.data, coef_names){
  if (all(coef_names %in% colnames(.data)) == FALSE){
    stop("Names of coefficients for the second stage of a Fama-MacBeth
          regression should be columns in the data frame from the first stage")
  }

  # function to run on each coefficient (a column in .data)
  f_ts_reg <- function(tmp_name){
    tmp_y <- .data[[tmp_name]]

    tmp_fit <- stats::lm(tmp_y ~ 1)

    df_tmp <- broom::tidy(tmp_fit)

    df_tmp$term <- tmp_name

    df_tmp
  }

  # ts regressions
  l_ts_est <- lapply(coef_names, FUN = f_ts_reg)

  # fast rbind
  df_ts_est <- dplyr::bind_rows(l_ts_est)

  df_ts_est
}
