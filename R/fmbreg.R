###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Fama-MacBeth regression.
#'
#' Estimates Fama-MacBeth regression.
#'
#' @param .data     Data frame with the data
#' @param y         Name of the dependent variable
#'                  (should be a colname in .data)
#' @param X         Names of the regressors
#'                  (should be colnames in .data)
#' @param date_var  Name of the date variable
#'                  (should be a colname in .data)
#' @param intercept Logical/Double: If FALSE, no intercept.
#'                                  If TRUE, intercept.
#'                                  If number, the column of this number is included
#'                                             instead of column of ones.
#'
#' @return A list: $est      - vector of estimates of the coefficients;
#'                 $t_stat   - vector of the t-stats for the coefficients;
#'                 $full_est - data frame with cross-sectional estimates
#'                             for every period.

#' @export
fmbreg <- function(.data, y, X, date_var, intercept = TRUE){
  # check arguments
  stopifnot(AreCharacters(y, X, date_var))
  stopifnot(AreInColnames(.data, y, X, date_var))

  stopifnot(length(y) == 1)
  stopifnot(length(date_var) == 1)
  stopifnot(length(intercept) == 1)

  # make unique dates
  v_dates <- sort(unique(.data[[date_var]]))
  n_dates <- length(v_dates)

  # prepare list for results
  l_res <- vector(mode = "list", length = n_dates)
  names(l_res) <- v_dates

  R2FromLmFit <- mystats::R2FromLmFit

  # estimate the model for each time period
  for (i in seq(1, n_dates)){
    df_tmp <- .data[.data[[date_var]] == v_dates[i], ]

    if (nrow(df_tmp) < 100){
      warning("Date ", v_dates[i], " contains less than 100 observations")
    }

    m_X <- as.matrix(df_tmp[X])
    if ((intercept != FALSE) && (intercept != 0)){
      m_X <- cbind(`(Intercept)` = rep(as.double(intercept), times = nrow(m_X)),
                   m_X)
    }

    tmp_obj <- stats::lm.fit(x = m_X, y = df_tmp[[y]])
    
    l_res[[i]] <- list(coefs = tmp_obj$coefficients,
                       R2 = R2FromLmFit(tmp_obj))
  }

  # extract R^2
  v_from_ll <- portfs::v_from_ll
  
  v_R2 <- v_from_ll(l_res, key = "R2")

  # extract coefs
  fGet <- function(l_elem){
    l_elem[["coefs"]]
  }
  m_coefs <- t(vapply(l_res, FUN = fGet, FUN.VALUE = l_res[[1]][["coefs"]]))

  stopifnot(nrow(m_coefs) == n_dates)
  stopifnot(all(X %in% colnames(m_coefs)))

  stopifnot(names(v_R2) == as.character(v_dates))
  stopifnot(rownames(m_coefs) == as.character(v_dates))

  # make data frame with cross sectional estimates
  df_full_est <- data.frame(date = v_dates, R2 = v_R2)
  colnames(df_full_est)[1] <- date_var
  df_full_est <- cbind(df_full_est, m_coefs)

  # make Fama-MacBeth estimates
  k <- ncol(m_coefs)
  
  v_est <- rep(NA, times = k)
  names(v_est) <- colnames(m_coefs)

  v_t_stat <- rep(NA, times = k)
  names(v_t_stat) <- colnames(m_coefs)

  for (j in seq(1, k)){
    tmp_y <- m_coefs[, j]
    tmp_res <- stats::lm(tmp_y ~ 1)
    tmp_coef <- stats::coef(summary(tmp_res))
    v_est[j]    <- tmp_coef["(Intercept)", "Estimate"]
    v_t_stat[j] <- tmp_coef["(Intercept)", "t value"]
  }

  # return
  list(est    = v_est,
       t_stat = v_t_stat,
       full_est = df_full_est)
}
