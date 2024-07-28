test_that("pred_weights function handles various scenarios correctly", {
  # EXAMPLE DATA
  group_df <- data.frame(
    group = c(0, 1, 0, 1),
    v1 = c(1.5, -1.6, 0.2, 0.3),
    v2 = c(0.6, 0.5, 1.4, 1.5),
    v3 = c(-0.3, 0.2, -1.4, -1.0)
  )

  pre_post_df <- data.frame(
    time = c(0, 0, 6, 6),
    id = c(1, 2, 1, 2),
    v1 = c(1.5, -1.6, 0.2, 0.3),
    v2 = c(0.6, 0.5, 1.4, 1.5),
    v3 = c(-0.3, 0.2, -1.4, -1.0)
  )

  # EXPECT ERRORS
  # Ensure 'id', 'pre', and 'post' are provided for 'prepost' type

  # Test invalid type specification
  expect_error(
    pred_weights(dataset = group_df, vars = c('v1', 'v3'), gtvar = 'group', type = 'Jango'),
    "type arugment must be 'group' or 'prepost'"
  )

  # corr_method needs to be valid
  expect_error(
    pred_weights(dataset = group_df, vars = c('v1', 'v3'), gtvar='group', corr_method='Force'),
    "corr_method must be one of: 'pearson', 'kendall', or 'spearman'"
  )

  # vars need to be valid col vars in data frame
  expect_error(filter_by_time_var(pred_weights(dataset=group_df, vars=c('v1', 'clone'),)),
               "All elements in 'vars' must be valid column names in the data frame.")
  # EXPECT EQUAL
  # Verify correct weights for group type
  weights_group <- pred_weights(dataset = group_df, vars = c('v1', 'v3'), type = 'group')
  expect_equal(weights_group, c(v1 = 0.86083556, v3 = 0.86083556))

  # Verify correct weights for prepost type
  weights_prepost <- pred_weights(dataset = pre_post_df, vars = c('v1', 'v3'), gtvar = 'time',
                                 type = 'prepost', pre = 0, post = 6, id = 'id')
  expect_equal(weights_prepost, c(v1 = 0.5, v3 = 0.5))

})
