###############################################################################
# (c) Maxim Sokolov
###############################################################################

lhs_name_ <- function(model){
  if (length(model$y) != 1){
    stop("The model should contain exactly one LHS variable.")
  }

  model$y
}
