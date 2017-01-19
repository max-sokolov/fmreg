###############################################################################
# (c) Maxim Sokolov
###############################################################################

# S3 methods for class "fmreg"

tidy.fmreg <- function(obj){
  obj$fm_estimates
}

coef.fmreg <- function(obj){
  fm_estimates <- tidy.fmreg(obj)

  v_coef <- fm_estimates[, "estimate", drop = TRUE]
  names(v_coef) <- fm_estimates[, "term", drop = TRUE]

  v_coef
}
