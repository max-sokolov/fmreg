###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Fama-MacBeth regression.
#'
#' \code{fmbreg} estimates Fama-MacBeth regression.
#'
#' @param .data     Data frame with the data
#' @param y         Name of the dependent variable
#'                  (should be a colname in .data)
#' @param X         Names of the regressors
#'                  (should be colnames in .data)
#' @param date_var  Name of the date variable
#'                  (should be a colname in .data)
#' @param intercept Logical/Double: If FALSE, no intercept.
#'                                  If TRUE, add intercept.
#'                                  If intercept = a,
#'                                  the constant regressor is of form
#'                                  rep(a, times = N) instead of rep(1L, times = N).
#'
#' @return A list: $fmb_estimates - data frame with Fama-MacBeth estimates;
#'                 $cs_estimates  - data frame with cross-sectional estimates
#'                                  for every period.

#' @export
fmbreg <- function(.data, y, X, date_var, intercept = TRUE){
  # ___________________ check arguments ____________________
  if (are_characters(y, X, date_var) == FALSE){
    stop("Arguments y, X, and date_var need to be character vectors.")
  }

  if (all(c(y, X, date_var) %in% colnames(.data)) == FALSE){
    stop("Names in y, X, and date_var need to be names from .data colnames.")
  }

  # small function
  f_require_length_one <- function(arg_name){
    if (length(arg_name) != 1){
      stop(arg_name, "is supposed to be of length 1.")
    }
  }

  # apply the small function
  f_require_length_one(y)
  f_require_length_one(date_var)
  f_require_length_one(intercept)

  # __________________ augment regressors __________________
  if (intercept == FALSE){
    X_aug <- X
  } else {
    if ("(Intercept)" %in% colnames(.data)){
      stop("If you want to include intercept in the model,
            .data should not contain a column named '(Intercept)'.")
    }

    .data$`(Intercept)` <- as.double(intercept)

    X_aug <- c("(Intercept)", X)
  }

  k <- length(X_aug)

  # __________________ make unique dates ___________________
  v_dates <- sort(unique(.data[[date_var]]))
  n_dates <- length(v_dates)

  # _____________ cross-sectional regressions ______________

  R2FromLmFit <- mystats::R2FromLmFit

  # function that estimates the model for a cross-section given by the date
  f_estimate <- function(tmp_date){
    df_tmp <- .data[.data[[date_var]] == tmp_date, ]

    if (nrow(df_tmp) < 100){
      warning("Date ", tmp_date, " contains less than 100 observations")
    }

    m_X <- as.matrix(df_tmp[, X_aug])

    tmp_fit <- stats::lm.fit(x = m_X, y = df_tmp[[y]])

    list(coefficients = tmp_fit$coefficients,
         r.squared    = R2FromLmFit(tmp_fit))
  }

  l_cs_est <- lapply(v_dates, FUN = f_estimate)

  # extract R^2
  v_from_ll <- portfs::v_from_ll
  
  v_r_squared <- v_from_ll(l_cs_est, key = "r.squared")

  # extract coefs
  fGet <- function(l_elem){
    l_elem[["coefficients"]]
  }

  m_coefs <- t(vapply(l_cs_est,
                      FUN = fGet,
                      FUN.VALUE = double(k))
              )

  stopifnot(nrow(m_coefs) == n_dates)
  stopifnot(all(X %in% colnames(m_coefs)))

  stopifnot(names(v_r_squared) == as.character(v_dates))
  stopifnot(rownames(m_coefs)  == as.character(v_dates))

  # make data frame with cross sectional estimates
  df_cs_est <- data.frame(date = v_dates,
                          r.squared = v_r_squared)

  colnames(df_cs_est)[1] <- date_var
  df_cs_est <- cbind(df_cs_est, m_coefs)

  # make Fama-MacBeth estimates
  term_names <- colnames(m_coefs)

  df_fmb_est <- list()

  for (j in seq(1, k)){
    tmp_y <- m_coefs[, j]

    tmp_fit <- stats::lm(tmp_y ~ 1)

    df_tmp <- broom::tidy(tmp_fit)

    df_tmp$term <- term_names[j]

    df_fmb_est <- rbind(df_fmb_est, df_tmp)
  }

  # return
  list(fmb_estimates = df_fmb_est,
       cs_estimates  = df_cs_est)
}
