###############################################################################
# (c) Maxim Sokolov
###############################################################################

x_matrix_ <- function(.data, model){
  intercept <- model$intercept

  # if there is intercept add column of ones to the data
  if (is.null(intercept) == FALSE){
    if (intercept == TRUE){
      if ('(Intercept)' %in% colnames(.data)){
        stop("'.data' should not have a column '(Intercept)'
              if 'intercept' == TRUE")
      } else {
        .data$`(Intercept)` <- 1L
      }
    } else if (intercept != FALSE){
      stop("'intercept' should be TRUE or FALSE")
    }
  }

  as.matrix(.data[, rhs_names_(model), drop = FALSE])
}
