test_that("pred_adjusted works correctly", {

  # pred_adjusted <- function(adj_ex, 'increase', c('v1', 'v3'), 'sex, 'group', 0)
  adj_ex <- data.frame(
    group = c(1, 0, 0, 1, 1, 0, 0, 0, 1, 1),
    sex = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
    v1 = c(-0.7046424, 0.6421641, -0.6173519, 2.4181569, 3.1563613, 2.2968339, 0.8278949, 2.2867542, 4.8919046, 2.9145953),
    v2 = c(-0.3150675, -0.7102407, -1.5986037, -3.2377127, -5.3377156, 3.1503806, 2.1361708, 1.1804201, -1.6361040, -0.6948423),
    v3 = c(-1.6282994, -0.4481524, 0.5327039, 5.6505460, 4.0647423, 1.3416172, 1.5042485, 2.0283717, 3.9714711, 6.3953932),
    strs = c('S', 'k','y','w','a','l', 'k', 'e', 'r', 's')
  )

  # EXPECT ERRORS
  expect_error(pred_adjusted(adj_ex, "increase", c('v1', 'v4'), 'sex', "group", 0),
               "All elements in vars must be column variables in the dataset")
  expect_error(pred_adjusted(adj_ex, "increase", c('v1', 'v2'), 'Doodku', "group", 0),
               "All elements in covariates must be column variables in the dataset")
  expect_error(pred_adjusted(adj_ex, "increase", c('v1', 'v2'), 'sex', "midichlorian", 0),
               "group must be a column variable in the dataset")
  expect_error(pred_adjusted(adj_ex, "increase", c('v1', 'v2'), 'sex', "group", 2),
               "Reference value must be present in the group variable")
  expect_error(pred_adjusted(adj_ex, "increase", c('v1', 'v2', 'group'),
                             'sex', "group", 0), "group must be disjoint from vars")
  expect_error(pred_adjusted(adj_ex, "increase", c('v1', 'v2'), c('v2', 'v3', 'group'),
                             "group", 0), "group must be disjoint from covariates")
  expect_error(pred_adjusted(adj_ex, "increase", c('v1', 'v2'), c('v2', 'strs'),
                             "group", 0), "All covariates must be numeric")
  expect_error(pred_adjusted(adj_ex, "increase", c('strs', 'v2'), 'sex', "group", 0),
               "All elements in vars must be numeric")

  expect_error(pred_adjusted(adj_ex, "R4", c('v1', 'v2'), 'sex', "group", 0),)

  expect_error(pred_adjusted(adj_ex, c("increase", "decrease", "ewok"), 'sex', 'group',
                             "group", 0),)

  # EXPECT EQUAL
  result <- pred_adjusted(adj_ex, 'increase', c('v1', 'v3'), 'sex', 'group', 0)
  expect_equal(result$results, c(v1 = 1, v3 = 1))
  expect_equal(result$weights, c(v1 = 0.7492363, v3 = 0.7492363), tolerance = 1e-7)

})
