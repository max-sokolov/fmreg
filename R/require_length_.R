###############################################################################
# (c) Maxim Sokolov
###############################################################################

require_length_ <- function(x, len){
  if (length(x) != len){
    stop("Variable ", deparse(substitute(x)),
         " is supposed to be of length ", len, ".")
  }

  invisible(TRUE)
}
