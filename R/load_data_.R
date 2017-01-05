###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Load data for specified date.
#'
#' \code{load_data_} takes only part of the data that corresponds to
#' a specified date.
#'
#' @param .data     "Data frame - like" structure.
#'                  \code{.data} can be a data frame, a data table,
#'                  or a tibble, including a "connection" to
#'                  a out-of-memory database.
#' @param .date     Date that is used to filter the data.
#' @param date_var  Name of the date variable (should be a colname in .data)
#'
#' @return Part (rows) of \code{.data} that corresponds to \code{.date}.
#'

#' @import data.table
#' @keywords internal
load_data_ <- function(.data, .date, date_var){
  # general checks
  require_length_(.date, 1)
  require_length_(date_var, 1)

  if (date_var %in% colnames(.data) == FALSE){
    stop("'date_var' need to be a name of a column of .data.")
  }

  # main part
  if (inherits(.data, "data.table")){
    # filter on data.table can be done very fast

    if (data.table::haskey(.data) && data.table::key(.data)[1] == date_var){
      # .data is already sorted by date_var

      dt_out <- .data[list(.date), ]

    } else if (date_var %in% data.table::indices(.data)){
      # date_var is an index for .data

      dt_out <- .data[list(.date), on = date_var]

    } else {
      # base case: not any faster than an ordinary data frame

      dt_out <- .data[.data[[date_var]] == .date, ]

    }

    df_out <- as.data.frame(dt_out)

  } else if (inherits(.data, "tbl_sql")){
    # work with out-of-memory tibble

    templ_df_out <- dplyr::filter_(.data,
                                   lazyeval::interp(~ x == .date,
                                                    x = as.name(date_var)))

    df_out <- dplyr::collect(templ_df_out, n = Inf)

  } else {
    # base case: data frame

    df_out <- .data[.data[[date_var]] == .date, , drop = FALSE]

  }

  df_out
}
