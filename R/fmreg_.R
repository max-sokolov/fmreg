###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Advanced Fama-MacBeth regression.
#'
#' \code{fmreg_} estimates Fama-MacBeth regressions on many specifications
#' "in parallel".
#'
#' @param .data        "Data frame - like" structure.
#'                     \code{.data} can be a data frame, a data table,
#'                     or a tibble, including a "connection" to
#'                     a out-of-memory database.
#'
#' @param models       A list of models.
#'                     Each model specifies the model to be estimated.
#'
#' @param dates        A vector of dates that are included in the estimation
#'                     of the regression.
#'
#' @param date_var     Name of the date variable
#'                     (should be a colname in \code{.data}).
#'
#' @param load_data    Function of the form function(.data, .date, date_var)
#'                     that returns a data frame.
#'                     This function is used to get the "slice" of \code{.data}
#'                     for \code{.date}.
#'                     If NULL, the internal function is used.
#'
#' @param prepare_data Function of the form function(.data, .date) that returns
#'                     a data frame.
#'                     This function modifies the "slice" of data loaded with
#'                     \code{load_data} before the first stage regression is
#'                     done on this "slice" of data.
#'                     If NULL, this step is skipped.
#'
#' @param estimate     Function of the form function(model, .data, .date) that
#'                     returns a list of the first-stage estimates for
#'                     \code{.date}.
#'                     This function should produce results that
#'                     \code{aggregate} will be able to use for
#'                     the second stage.
#'                     If NULL, the internal function is used.
#'
#' @param aggregate    Function of the form function(model, l_fit) that returns
#'                     an \code{fmreg} object.
#'                     This function takes a \code{model} and a list of
#'                     the first stage estimates for the \code{model} and
#'                     does the second stage of the Fama-MacBeth regression.
#'                     If NULL, the internal function is used.
#'
#' @param progress     Logical: If TRUE, the progress of the estimation
#'                     is printed to the standard output.
#'
#' @return A list with of \code{fmreg} objects.
#'         Each object corresponds to the \code{model} from
#'         the \code{models} argument.
#'
#' @seealso \code{\link[fmreg]{fmreg}}

#' @export
fmreg_ <- function(.data, models, dates, date_var,
                   load_data = NULL, prepare_data = NULL,
                   estimate = NULL, aggregate = NULL,
                   progress = FALSE){

  # if user functions are not provided, use internal functions
  if (is.null(load_data)){
    load_data <- load_data_
  }

  if (is.null(estimate)){
    estimate <- estimate_
  }

  if (is.null(aggregate)){
    aggregate <- aggregate_
  }

  # function to run on each date
  do_for_date <- function(.date, .data){
    if (progress){
      cat("\n")
      cat("Estimation for date: ", .date, "\n")
    }

    df_tmp <- load_data(.data, .date = .date, date_var = date_var)

    if (is.null(prepare_data) == FALSE){
      df_tmp <- prepare_data(df_tmp, .date = .date)
    }

    models_fit  <- lapply(models,
                          FUN = estimate,
                          .data = df_tmp,
                          .date = .date)

    models_fit
  }

  # first step of the Fama-MacBeth procedure
  l_models_fit <- lapply(dates, FUN = do_for_date, .data = .data)

  # just for convenience:
  # list of dates of list of models -> list of models of list of dates
  models_l_fit <- purrr::transpose(l_models_fit)

  # second step of the Fama-MacBeth procedure
  models_fit <- purrr::map2(models, models_l_fit, .f = aggregate)

  models_fit
}
