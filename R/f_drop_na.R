###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Drop 'NA's.
#'
#' \code{drop_na} deletes NA cases in the specified vars from a data frame.
#'
#' @param .data       Data frame with the data.
#' @param filter_vars Names of the variables
#'                    for which NA cases should be deleted.

#' @keywords internal
drop_na <- function(.data, filter_vars){

  stopifnot(is.character(filter_vars))

  dots <- vector(mode = "list", length = length(filter_vars))

  for(i in seq_along(dots)){
    name <- filter_vars[i]

    if (name %in% names(.data) == FALSE){
      stop(name, " is not a colname of the data.frame")
    }

    dots[[i]] <- substitute(is.na(x) == FALSE,
                            env = list(x = as.name(name)))
  }

  dplyr::filter_(.data, .dots = dots)
}
