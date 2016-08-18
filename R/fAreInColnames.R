###############################################################################
# (c) Maxim Sokolov
###############################################################################

AreInColnames <- function(df_data, ...){
  
  l_args <- list(...)

  if (length(l_args) == 0){
    stop("provide at least one colname")
  }

  fAreInColnames <- function(v_names){
    all(v_names %in% colnames(df_data))
  }

  v_are_in_colnames <- vapply(l_args, FUN = fAreInColnames, FUN.VALUE = TRUE)

  all(v_are_in_colnames)
}
