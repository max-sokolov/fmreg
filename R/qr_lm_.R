# This is just a copy of the method qr.lm from stats package.

qr_lm_ <- function(x, ...){
  if (is.null(r <- x$qr)){
    stop("lm object does not have a proper 'qr' component.\n Rank zero or should not have used lm(.., qr=FALSE).")
  } 

  r
}