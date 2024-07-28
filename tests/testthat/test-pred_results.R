test_that("pred_results works as expected", {

  group_df <- data.frame(
    group = c('A', 'A', 'A', 'A', 'B', 'B', 'B', 'B'),
    v1 = c(1.5, 3.2, 5.8, 7.6, 1.6, 3.1, 6.3, 7.4),
    v2 = c(2.1, 4.5, 6.7, 8.9, 3.5, 5.6, 9.8, 13.1),
    v3 = c(0.5, 2.1, 3.8, 2.1, 0.3, 1.1, 0.7, 1.7)
  )

  time_df <- data.frame(
    ID = c(1, 2, 3, 4, 1, 2, 3, 4),
    time = c(0, 0, 0, 0, 6, 6, 6, 6),
    v1 = c(1.5, 3.2, 5.8, 7.6, 1.6, 3.1, 6.3, 7.4),
    v2 = c(2.1, 4.5, 6.7, 8.9, 3.5, 5.6, 9.8, 13.1),
    v3 = c(0.5, 2.1, 3.8, 2.1, 0.3, 1.1, 0.7, 1.7)
  )

  # this is just a wrapper function that calls prevoiusly tested functions


  # EXPECT EQUALS

  # time_df
  expect_equal(
    pred_results(dataset = time_df, vars = c('v1', 'v3'), type = 'prepost', id ="ID", gtvar="time",
                 grp_a = 0, grp_b = 6, hypothesis = 'increase'),
    list(
      results = c(1, 0),
      differences = c(0.2, -1.2),
      variables = c("v1", "v3")
    )
  )


  # group_df

  expect_equal(
    pred_results(dataset=group_df, vars=c('v1', 'v2', 'v3'), type='group',
                 hypothesis='increase', gtvar='group', grp_a='A', grp_b='B'),
    list(
      results = c(1, 1, 0),
      differences = c(0.2, 2.1, -1.2),
      variables = c("v1", "v2", "v3")
    )
  )



})
