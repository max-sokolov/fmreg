###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Winsorize.
#'
#' Winsorizes a vector at given levels of cutoffs.
#'
#' @param x       A vector that should be winsorized.
#' @param cutoffs A vector with cutoffs.
#'                E.g., cutoffs = c(0.01, 0.99) means
#'                      "winsorize at 1\% and 99\% levels".

winsorize <- function(x, cutoffs){
  stopifnot(length(cutoffs) == 2)
  stopifnot(all(cutoffs >= 0 & cutoffs <= 1))
  stopifnot(cutoffs[1] <= cutoffs[2])

  quants <- stats::quantile(x, probs = cutoffs, na.rm = TRUE, type = 1)

  low <- quants[1]
  up  <- quants[2]

  x[x < low] <- low
  x[x > up]  <- up

  x
}
