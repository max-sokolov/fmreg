###############################################################################
# (c) Maxim Sokolov
###############################################################################

# _____________________________________________________________________________
context("Main check for load_data_()")

df_data <- data.frame(a = c(1L, 2L, 2L),
                      b = c(1,  2,  3))

test_that("f works for the base case (data frame)", {
  # one row
  expect_identical(load_data_(df_data, .date = 1L, date_var = "a"),
                   data.frame(a = c(1L),
                              b = c(1),
                              row.names = c(1L)
                             )
                  )

  # two rows
  expect_identical(load_data_(df_data, .date = 2L, date_var = "a"),
                   data.frame(a = c(2L, 2L),
                              b = c(2, 3),
                              row.names = c(2L, 3L)
                             )
                  )

  # two rows, one column
  expect_identical(load_data_(df_data[ , "a", drop = FALSE],
                              .date = 2L,
                              date_var = "a"),
                   data.frame(a = c(2L, 2L),
                              row.names = c(2L, 3L)
                             )
                  )
})

test_that("f works for data table (the base case)", {
  dt_data <- data.table::as.data.table(df_data)

  # one row
  df_exp <- as.data.frame(data.table::data.table(a = c(1L),
                                                 b = c(1))
                         )
  expect_identical(load_data_(dt_data, .date = 1L, date_var = "a"),
                   df_exp)

  # two rows
  df_exp <- as.data.frame(data.table::data.table(a = c(2L, 2L),
                                                 b = c(2, 3))
                         )
  expect_identical(load_data_(dt_data, .date = 2L, date_var = "a"),
                   df_exp)

  # two rows, one column
  df_exp <- as.data.frame(data.table::data.table(a = c(2L, 2L)))

  expect_identical(load_data_(dt_data[, list(a)], .date = 2L, date_var = "a"),
                   df_exp)
})

test_that("f works for data table (the key case)", {
  dt_data <- data.table::as.data.table(df_data)
  data.table::setkey(dt_data, a)

  # one row
  # expect_equivalent() ignores attributes
  df_exp <- as.data.frame(data.table::data.table(a = c(1L),
                                                 b = c(1))
                         )
  expect_identical(load_data_(dt_data, .date = 1L, date_var = "a"),
                   df_exp)

  # two rows
  df_exp <- as.data.frame(data.table::data.table(a = c(2L, 2L),
                                                 b = c(2, 3))
                         )
  expect_equivalent(load_data_(dt_data, .date = 2L, date_var = "a"),
                    df_exp)

  # two rows, one column
  df_exp <- as.data.frame(data.table::data.table(a = c(2L, 2L)))

  expect_equivalent(load_data_(dt_data[, list(a)], .date = 2L, date_var = "a"),
                    df_exp)
})

test_that("f works for data table (the index case)", {
  dt_data <- data.table::as.data.table(df_data)
  data.table::setindex(dt_data, a)

  # one row
  # expect_equivalent() ignores attributes
  df_exp <- as.data.frame(data.table::data.table(a = c(1L),
                                                 b = c(1))
                         )
  expect_identical(load_data_(dt_data, .date = 1L, date_var = "a"),
                   df_exp)

  # two rows
  df_exp <- as.data.frame(data.table::data.table(a = c(2L, 2L),
                                                 b = c(2, 3))
                         )
  expect_equivalent(load_data_(dt_data, .date = 2L, date_var = "a"),
                    df_exp)

  # two rows, one column
  df_exp <- as.data.frame(data.table::data.table(a = c(2L, 2L)))

  expect_equivalent(load_data_(dt_data[, list(a)], .date = 2L, date_var = "a"),
                    df_exp)
})

test_that("f works for sqlite", {
  ############## prepare database: CHANGES THE CURRENT DIRECTORY ##############
  tmp_name <- "tmp_db.sqlite3"

  db_data <- dplyr::src_sqlite(tmp_name, create = TRUE)

  tab_data <- dplyr::copy_to(db_data, df_data)

  tab_data_1col <- dplyr::copy_to(db_data, df_data[, "a", drop = FALSE])

  ############################### do the tests ################################
  # one row
  expect_identical(load_data_(tab_data, .date = 1L, date_var = "a"),
                   dplyr::data_frame(a = c(1L),
                                     b = c(1))
                  )

  # two rows
  expect_identical(load_data_(tab_data, .date = 2L, date_var = "a"),
                   dplyr::data_frame(a = c(2L, 2L),
                                     b = c(2, 3))
                  )

  # two rows, one column
  expect_identical(load_data_(tab_data_1col, .date = 2L, date_var = "a"),
                   dplyr::data_frame(a = c(2L, 2L))
                  )

  ############################## delete database ##############################
  file.remove(tmp_name)
})

