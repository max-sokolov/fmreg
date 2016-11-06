###############################################################################
# (c) Maxim Sokolov
###############################################################################

mutate_cs_ <- function(.data, vars, date_var, method, cutoffs,
                       out_vars = vars){
  # _________________ check arguments ______________________
  stopifnot(are_characters_(vars, date_var, method))

  stopifnot(all(c(vars, date_var) %in% colnames(.data)))

  stopifnot(length(date_var) == 1)

  stopifnot(length(vars) == length(out_vars))

  # _____________________ mutate ___________________________
  # mutating function
  f_mutate <- function(x){
    modify_tails_(x, cutoffs = cutoffs, method = method)
  }

  # standard evaluation machinery for mutate_
  dots <- list()
  for (i in seq_along(vars)){
    dots[[ out_vars[i] ]] <- lazyeval::interp(~ f_mutate(x),
                                              x = as.name(vars[i]))
  }

  # do mutate for each cross-section separately!
  df_data_out <- dplyr::group_by_(.data, .dots = list(as.name(date_var)))

  df_data_out <- dplyr::mutate_(df_data_out, .dots = dots)

  df_data_out <- dplyr::ungroup(df_data_out)

  df_data_out
}
