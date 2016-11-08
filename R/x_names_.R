###############################################################################
# (c) Maxim Sokolov
###############################################################################

x_names_ <- function(x, intercept){
  if (intercept == TRUE){
    if ('(Intercept)' %in% x){
      stop("'x' cannot include '(Intercept)' if 'intercept' == TRUE")
    } else {
      out <- c("(Intercept)", x)
    }
  } else {
    out <- x
  }

  out
}
