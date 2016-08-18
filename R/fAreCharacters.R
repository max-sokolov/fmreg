###############################################################################
# (c) Maxim Sokolov
###############################################################################

AreCharacters <- function(...){
  
  l_args <- list(...)

  if (length(l_args) == 0){
    stop("provide at least one argument")
  }

  v_is_character <- vapply(l_args, FUN = is.character, FUN.VALUE = TRUE)

  all(v_is_character)
}
