###############################################################################
# (c) Maxim Sokolov
###############################################################################

mutate_cs <- function(.data, vars, date_var, method, cutoffs,
                      out_vars = vars){
  # _________________ check arguments ______________________
  stopifnot(are_characters(vars, date_var, method))

  stopifnot(all(c(vars, date_var) %in% colnames(.data)))

  stopifnot(length(date_var) == 1)

  stopifnot(length(vars) == length(out_vars))

  # ________________ choose the method _____________________
  if (method == "winsorize"){
    f_mutate <- quote(fmbreg:::winsorize)
  } else if (method == "trim"){
    f_mutate <- quote(fmbreg:::trim)
  } else {
    stop("The `method` argument is illegal")
  }

  # standard evaluation machinery for mutate_
  dots <- list()
  for (i in seq_along(vars)){
    dots[[ out_vars[i] ]] <- substitute(f_mutate(x, cutoffs = cutoffs),
                                        env = list(x = as.name(vars[i]),
                                                   cutoffs = cutoffs,
                                                   f_mutate = f_mutate))
  }

  df_data_out <- dplyr::group_by_(.data, .dots = list(as.name(date_var)))

  df_data_out <- dplyr::mutate_(df_data_out, .dots = dots)

  df_data_out <- dplyr::ungroup(df_data_out)

  df_data_out
}
