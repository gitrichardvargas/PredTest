
test_that("filter_by_group_var works as expected", {
  # sample data frame
  mini_example <- data.frame(
    group = c('a', 'b', 'a', 'b'),
    v1 = c(1.5, -1.6, 0.2, 0.3),
    v2 = c(0.6, 0.5, 1.4, 1.5),
    v3 = c(-0.3, 0.2, -1.4, -1.0)
  )

  # EXPECT ERRORS

  # Error if vars not in data frame
  expect_error(filter_by_group_var(mini_example, "group", 'a','b', c("v1", "X-Wing")),
               "All elements in 'vars' must be valid column names in the data frame.")

  # Error if df isn't a data frame
  expect_error(filter_by_group_var("Strings aren't data frames", "group", 'a', 'b', c("v1", "v3")),
               "The input 'df' must be a data frame.")

  # Error if grp_var isn't a column variable
  expect_error(filter_by_group_var(mini_example, "TIE fighter", 'a', 'b', c("v1", "v3")),
               "The specified 'grp_var' is not a column in the data frame.")

  # Error if grp_1 or grp_2 are not valid row variables in grp_var
  expect_error(filter_by_group_var(mini_example, "group", 'c', 'd', c("v1", "v3")),
               "Both 'grp_1' and 'grp_2' must be valid elements in the specified 'grp_var' column.")

  # Testing correct function execution
  output = filter_by_group_var(mini_example, "group", 'a', 'b', c("v1", "v3"))

  # Expected data frames
  grp_1_expected_df = data.frame(v1 = c(1.5, 0.2), v3 = c(-0.3, -1.4))
  grp_2_expected_df = data.frame(v1 = c(-1.6, 0.3), v3 = c(0.2, -1.0))


  # EXPECT EQUAL

  # Test the return value
  expect_true(is.list(output), "Output should be a list")
  # Groups from output have the same columns
  expect_equal(names(output$group_1), names(output$group_2))

  # See if expected_dfs equal dfs from output
  expect_equal(grp_1_expected_df$v1, output$group_1$v1)
  expect_equal(grp_1_expected_df$v3, output$group_1$v3)
  expect_equal(grp_2_expected_df$v1, output$group_2$v1)
  expect_equal(grp_2_expected_df$v3, output$group_2$v3)

})
