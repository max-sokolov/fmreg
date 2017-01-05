###############################################################################
# (c) Maxim Sokolov
###############################################################################

rhs_names_ <- function(model){
  intercept <- model$intercept
  x         <- model$x

  if(is.null(intercept)){
    # no intercept is treated as "don't include an intercept"
    intercept <- FALSE
  }

  # cases
  if (intercept == TRUE){
    if ('(Intercept)' %in% x){
      stop("'x' cannot include '(Intercept)' if 'intercept' == TRUE.")
    }
    
    rhs_names <- c("(Intercept)", x)
    
  } else if (intercept == FALSE){
    if (is.null(x)){
      stop("The model should contain at least one RHS variable.")
    }

    rhs_names <- x

  } else {
    stop("'intercept' should be TRUE or FALSE")
  }

  rhs_names
}
