###############################################################################
# (c) Maxim Sokolov
###############################################################################

#' Modify tails of a vector.
#'
#' \code{modify_tails} modify a vector by winsorizing or trimming the tails.
#'
#' @param x       A vector that should be modified.
#' @param cutoffs A vector with cutoffs.
#'                E.g., cutoffs = c(0.01, 0.99) means
#'                      "winsorize/trim at 1\% and 99\% levels".
#' @param method  Name of the modification method to be applied to the vector.
#'                Supports two methods: "winsorize" and "trim".

modify_tails <- function(x, cutoffs, method = c("winsorize", "trim")[1]){
  stopifnot(length(cutoffs) == 2)
  stopifnot(all(cutoffs >= 0 & cutoffs <= 1))
  stopifnot(cutoffs[1] <= cutoffs[2])
  stopifnot(length(method) == 1)

  quants <- stats::quantile(x, probs = cutoffs, na.rm = TRUE, type = 1)

  low <- quants[1]
  up  <- quants[2]

  if (method == "winsorize"){
    x[x < low] <- low
    x[x > up]  <- up
  } else if (method == "trim"){
    x[x < low] <- NA
    x[x > up]  <- NA
  } else {
    stop("modify_tails() supports only two methods: 'winsorize' and 'trim'.")
  }

  x
}
