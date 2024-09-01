
test_that("Testing create_difference_vector", {
  # EXAMPLE DATA
  df_1 = data.frame(
    v1 = c(1, 2, 3),
    v2 = c(4, 5, 6),
    v3 = c(7, 8, 9)
  )
  df_2 = df_1 + 2 # adds 2 to all values in df_1, shifts by 2
  df_smaller = data.frame(
    v1 = c(1, 2, 3),
    v2 = c(4, 5, 6)
  )
  mat_1 = as.matrix(df_1)
  mat_2 = mat_1 + 2

  # TESTING EXPECTED ERRORS

  # Error if grp_1_data isn't numeric
  df_non_numeric = data.frame(v1 = c("a", "b", "c"), v2 = c("d", "e", "f"), v3 = c("g", "h", "i"))
  expect_error(create_difference_vector(df_non_numeric, df_2),
               "All columns in grp_1_data and grp_2_data must be numeric.")

  # Error if grp_2_data isn't numeric
  expect_error(create_difference_vector(df_1, df_non_numeric),
               "All columns in grp_1_data and grp_2_data must be numeric.")

  # Error if grp_1_data and grp_2_data aren't the same size (same number of columns)
  expect_error(create_difference_vector(df_1, df_smaller),
               "grp_1_data and grp_2_data must have the same number of columns.")

  # Error if grp_1_data or grp_2_data is a data frame, but the other isn't already covered by type consistency test

  # EXPECT EQUAL
  # Testing correct function execution for mean
  expect_equal(c(2, 2, 2), create_difference_vector(df_1, df_2, location='mean'))
  expect_equal(c(2, 2, 2), create_difference_vector(mat_1, mat_2, location='mean'))

  # Testing correct function execution for median
  expect_equal(c(2, 2, 2), create_difference_vector(df_1, df_2, location='median'))
  expect_equal(c(2, 2, 2), create_difference_vector(mat_1, mat_2, location='median'))
})
