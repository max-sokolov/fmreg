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
#'
#' @seealso \code{\link[fmreg]{fmreg}}

#' @export
fmreg_ <- function(.data, y, x, date_var, intercept = TRUE,
                   winsorize = FALSE, trim = FALSE, cutoffs = c(0.01, 0.99),
                   min_obs = 100){

  # ____________________________ check arguments ______________________________
  if (are_characters(y, x, date_var) == FALSE){
    stop("Arguments y, X, and date_var need to be character vectors.")
  }

  if (all(c(y, x, date_var) %in% colnames(.data)) == FALSE){
    stop("Names in y, X, and date_var need to be names from .data colnames.")
  }

  # check the length of the args
  require_length(y, 1)
  require_length(date_var, 1)
  require_length(intercept, 1)
  require_length(winsorize, 1)
  require_length(trim, 1)
  require_length(cutoffs, 2)

  if (all(cutoffs >= 0 & cutoffs <= 1) == FALSE){
    stop("Cutoffs should be between 0 and 1.")
  }

  if (cutoffs[1] > cutoffs[2]){
    stop("cutoffs[1] should be less or equal to cutoffs[2].")
  }

  # _______________________________ prepare data ______________________________
  .data <- prepare_data_(.data, y = y, x = x, date_var = date_var,
                         winsorize = winsorize, trim = trim, cutoffs = cutoffs)

  # ____________________________ augment regressors ___________________________
  if (intercept == FALSE){
    x_aug <- x
  } else {
    if ("(Intercept)" %in% colnames(.data)){
      stop("If you want to include intercept in the model,
            .data should not contain a column named '(Intercept)'.")
    }

    .data$`(Intercept)` <- as.double(intercept)

    x_aug <- c("(Intercept)", x)
  }

  n_regressors <- length(x_aug)

  # ____________________________ make unique dates ____________________________
  v_dates <- sort(unique(.data[[date_var]]))
  n_dates <- length(v_dates)

  # _______________________ cross-sectional regressions _______________________
  # function that estimates the model for a cross-section given by the date
  f_estimate <- function(tmp_date){
    df_tmp <- .data[.data[[date_var]] == tmp_date, ]

    if (nrow(df_tmp) < min_obs){
      warning("Date ", tmp_date,
              " contains less than ", min_obs, " observations")
    }

    m_X <- as.matrix(df_tmp[, x_aug, drop = FALSE])

    tmp_fit <- stats::lm.fit(x = m_X, y = df_tmp[[y]])

    list(date         = tmp_date,
         r.squared    = r_squared_from_lm_fit(tmp_fit),
         coefficients = tmp_fit$coefficients)
  }

  l_cs_est <- lapply(v_dates, FUN = f_estimate)

  stopifnot(length(l_cs_est) == n_dates)

  # ________________ data frame with cross-sectional estimates ________________
  # function to access elements of the list
  f_get_elem <- function(l, elem){
    l[[elem]]
  }

  # start creating the data frame
  df_cs_est <- data.frame(date      = vapply(l_cs_est,
                                             FUN = f_get_elem,
                                             FUN.VALUE = l_cs_est[[1]]$date,
                                             elem = "date"),
                          r.squared = vapply(l_cs_est,
                                             FUN = f_get_elem,
                                             FUN.VALUE = double(1),
                                             elem = "r.squared"))

  # rename the date column
  names(df_cs_est)[1] <- date_var

  # sanity check
  stopifnot(nrow(df_cs_est) == n_dates)

  # prepare coefficients for adding to the data frame
  m_coefs <- t(vapply(l_cs_est,
                      FUN = f_get_elem,
                      FUN.VALUE = double(n_regressors),
                      elem = "coefficients")
              )

  stopifnot(nrow(m_coefs) == n_dates)
  stopifnot(ncol(m_coefs) == n_regressors)
  stopifnot(all(x_aug %in% colnames(m_coefs)))

  df_cs_est <- cbind(df_cs_est, m_coefs)

  # __________________________ Fama-MacBeth estimates _________________________
  for (j in seq_along(x_aug)){
    tmp_name <- x_aug[j]

    tmp_y <- df_cs_est[, tmp_name, drop = TRUE]

    tmp_fit <- stats::lm(tmp_y ~ 1)

    df_tmp <- broom::tidy(tmp_fit)

    df_tmp$term <- tmp_name

    if (j == 1){
      df_fm_est <- df_tmp
    } else {
      df_fm_est <- rbind(df_fm_est, df_tmp)
    }
  }

  # __________________________________ Return _________________________________
  list(fm_estimates = df_fm_est,
       cs_estimates  = df_cs_est)
}
