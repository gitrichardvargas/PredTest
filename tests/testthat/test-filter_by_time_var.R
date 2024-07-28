test_that("filter_by_time_var works as expected", {
  # sample data frame
  mini_example <- data.frame(
    time = c(0, 0, 6, 6), # perhaps it's checking results after 6 months
    id = c(1, 2, 1, 2), # ids would repeat for the second study
    v1 = c(1.5, -1.6, 0.2, 0.3),
    v2 = c(0.6, 0.5, 1.4, 1.5),
    v3 = c(-0.3, 0.2, -1.4, -1.0)
  )

  # Test for expected errors

  # Error if df isn't a data frame
  expect_error(filter_by_time_var("Strings aren't data frames", "time", 0, 6, c("v1", "v3")),
               "The input 'df' must be a data frame.")


  # Error if time_var isn't a column variable
  expect_error(filter_by_time_var(mini_example,"id", "nonexistent", 0, 6, c("v1", "v3")),
               "The specified 'time_var' is not a column in the data frame.")

  # Error if pre or post are not valid row variables in time_var
  expect_error(filter_by_time_var(mini_example, "id", "time", 2, 3, c("v1", "v3")),
               "Both 'pre' and 'post' must be valid elements in the specified 'time_var' column.")

  # Error if the number of rows in pre is not equal to the number of rows in post
  # This test needs to be in a situation where pre and post are intentionally unbalanced.
  # Here we'll manipulate mini_example for this test.
  mini_unbalanced = data.frame( # same as min_example, but there's no follow up on id 2
    time = c(0, 0, 6),
    id = c(1, 2, 1),
    v1 = c(1.5, -1.6, 0.2),
    v2 = c(0.6, 0.5, 1.4),
    v3 = c(-0.3, 0.2, -1.4)
  )



  # EXPECTED ERRORS
  expect_error(filter_by_time_var(mini_unbalanced,"id", "time", 0, 6, c("v1", "v3")),
               "pre and post should have an equal size")

  # Error if vars not in data frame
  expect_error(filter_by_time_var(mini_example, "id", "time", 0, 6, c("v1", "X-Wing")),
               "All elements in 'vars' must be valid column names in the data frame.")

  # Error if df isn't a data frame
  expect_error(filter_by_time_var("A string isn't a data frame", "id", "time", 0, 6, c("v1", "v3")),
               "The input 'df' must be a data frame.")

  # Error if time_var isn't a column variable
  expect_error(filter_by_time_var(mini_example, "id", "TIE fighter", 0, 6, c("v1", "v3")),
               "The specified 'time_var' is not a column in the data frame.")

  # Error if id isn't a column variable
  expect_error(filter_by_time_var(mini_example, "jedi", "time", 0, 6, c("v1", "v3")),
               "The specified 'id' is not a column in the data frame.")

  # Error if grp_1 or grp_2 are not valid row variables in time_var
  expect_error(filter_by_time_var(mini_example,"id", "time", 2, 3, c("v1", "v3")),
               "Both 'pre' and 'post' must be valid elements in the specified 'time_var' column.")

  # Testing correct function execution
  output = filter_by_time_var(mini_example,"id", "time", 0, 6, c("v1", "v3"))

  # Expected data frames
  pre_expected_df = data.frame(v1 = c(1.5, -1.6), v3 = c(-0.3, 0.2))
  post_expected_df = data.frame(v1 = c(0.2, 0.3), v3 = c(-1.4, -1.0))



  # Test the return value
  expect_true(is.list(output), "Output should be a list")

  # Test that both data frames have the same columns
  expect_equal(names(output$pre), names(pre_expected_df))
  expect_equal(names(output$post), names(post_expected_df))

  # Test that data frames are of the same size and shape
  expect_equal(dim(output$pre), dim(output$post))

  # See if expected_values equal dfs from output
  expect_equal(pre_expected_df$v1, output$pre$v1)
  expect_equal(pre_expected_df$v3, output$pre$v3)
  expect_equal(post_expected_df$v1, output$post$v1)
  expect_equal(post_expected_df$v3, output$post$v3)

})
