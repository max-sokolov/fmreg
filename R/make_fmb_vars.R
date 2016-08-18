###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Make Fama-MacBeth variables.
#'
#' Makes Fama-MacBeth variables for given names of variables
#' and adds them to the data frame.
#'
#' @param df_data  Data frame with the data.
#' @param vars     Character vector with names of variables
#'                 that are used to construct fmb_vars.
#' @param date_var A name of the variable with the dates (time dimension).
#' @param id_var   A name of the variable with the IDs (e.g., `permno`).
#' @param cutoffs  A vector with two cutoffs (lower and upper)
#'                 for winsorization or trimming.
#'                 E.g., c(0.01, 0.99) means 1\% and 99\% levels cutoffs.
#' @param method   A name of the method to use: "winsorize" or "trim".
#'
#' Note: The function rearranges the data
#'       by sorting on id_var and then on date_var.
#'       The function deletes all rows with NAs in fmb_vars.

#' @export
make_fmb_vars <- function(df_data, vars, date_var, id_var,
                          cutoffs = c(0.01, 0.99),
                          method = c("winsorize", "trim")[1]){

  # check arguments
  stopifnot(AreCharacters(vars, date_var, id_var, method))

  stopifnot(AreInColnames(df_data, vars, date_var, id_var))

  # remember the colnames
  v_init_colnames <- colnames(df_data)

  # make lag_vars
  lag_vars <- paste("lag_", vars, sep = "")

  # add lags
  AddLagVars <- portfs::AddLagVars

  df_data_lag <- AddLagVars(df_data,
                            vars = vars,
                            date_var = date_var,
                            id_var   = id_var,
                            lag_vars = lag_vars)

  # drop NAs
  DropNA <- portfs::DropNA

  df_data_lag <- DropNA(df_data_lag, filter_vars = lag_vars)

  # make fmb_vars
  fmb_vars <- stringr::str_replace(lag_vars,
                                   pattern = "lag",
                                   replacement = "fmb")

  WinsorizeOrTrimVars <- portfs:::WinsorizeOrTrimVars

  df_data_fmb <- WinsorizeOrTrimVars(df_data_lag,
                                     vars      = lag_vars,
                                     out_vars  = fmb_vars,
                                     group_var = date_var,
                                     method  = method,
                                     cutoffs = cutoffs)

  if (method == "trim"){
    # drop NAs
    df_data_fmb <- DropNA(df_data_fmb, filter_vars = fmb_vars)
  }

  # take only initial columns + fmb_vars
  df_data_fmb <- df_data_fmb[, c(v_init_colnames, fmb_vars)]

  df_data_fmb
}
