###############################################################################
# (c) Maxim Sokolov
###############################################################################

x_matrix_ <- function(.data, x, intercept){
  if (intercept == FALSE){
    # do nothing
  } else if (intercept == TRUE){
    if ('(Intercept)' %in% colnames(.data)){
      stop("'.data' should not have a column '(Intercept)'
            if 'intercept' == TRUE")
    } else {
      .data$`(Intercept)` <- 1L
    }
  } else {
    stop("'intercept' should be TRUE or FALSE")
  }

  as.matrix(.data[, x_names_(x, intercept), drop = FALSE])
}
