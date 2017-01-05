
#' R^2 from \code{lm.fit} output.
#'
#' \code{r_squared_from_lm_fit_} calculates R^2 (plain or adjusted)
#' from the output of \code{lm.fit}.
#'
#' @param obj       List: output from \code{lm.fit}.
#' @param intercept Logical: does the model contain an intercept?
#' @param is_adj    Logical: do you need the adjusted R^2?

#' @keywords internal
r_squared_from_lm_fit_ <- function(obj, intercept = TRUE, is_adj = FALSE){
  # rank
  p <- obj$rank
  # residual degrees of freedom
  rdf <- obj$df.residual
  
  # get qr object
  Qr <- qr_lm_(obj)
  # number of rows
  n <- NROW(Qr$qr)

  if (is.na(rdf) || n - p != rdf){ 
    warning("residual degrees of freedom in the object
             suggest this is not an 'lm' fit")
  }

  # residuals
  v_res <- obj$residuals
  # fitted values
  v_fit_vals <- obj$fitted.values

  # model sum of squares
  if (intercept == TRUE){
    mss <- sum((v_fit_vals - mean(v_fit_vals))^2)
  } else {
    mss <- sum(v_fit_vals^2)
  }

  # residual sum of squares
  rss <- sum(v_res^2)

  if (p == intercept){
    return(0)
  }

  # R^2
  r_squared <- mss/(mss + rss)

  if (is_adj == TRUE){
    adj_r_squared <- 1 - (1 - r_squared)*((n - intercept)/rdf)
    adj_r_squared
  } else {
    r_squared
  }
}
