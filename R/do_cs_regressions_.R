###############################################################################
# (c) Maxim Sokolov
###############################################################################

do_cs_regressions_ <- function(.data, y, x, date_var, intercept, min_obs){
  # unique dates
  v_dates <- sort(unique(.data[[date_var]]))

  # function to run on each date
  f_cs_reg <- function(tmp_date){
    df_tmp <- .data[.data[[date_var]] == tmp_date, ]

    if (nrow(df_tmp) < min_obs){
      warning("Date ", tmp_date,
              " contains less than ", min_obs, " observations")
    }

    m_x <- x_matrix_(df_tmp, x = x, intercept = intercept)

    tmp_fit <- stats::lm.fit(x = m_x, y = df_tmp[[y]])

    v_out <- c(date      = tmp_date,
               r.squared = r_squared_from_lm_fit(tmp_fit),
               tmp_fit$coefficients)

    # rename date
    names(v_out)[1] <- date_var

    m_out <- as.matrix(v_out)

    df_out <- as.data.frame(t(m_out))

    df_out
  }

  # cs regressions
  l_cs_est <- lapply(v_dates, FUN = f_cs_reg)

  # fast rbind
  df_cs_est <- dplyr::bind_rows(l_cs_est)

  df_cs_est
}
