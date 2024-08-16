test_that("solve_p0_score_ci works correctly", {
  trials <- 10
  successes <- 9
  p_hat <- successes / trials

  # EXPECT ERRORS
  # - p_hat is numeric and on the interval (0,1)
  expect_error(solve_p0_score_ci('Jar Jar', trials), "p_hat must be numeric")
  expect_error(solve_p0_score_ci(-1, trials))
  # - n is numeric and a natural number > 0
  expect_error(solve_p0_score_ci(p_hat, 'Binks'), "trials must be numeric.")
  expect_error(solve_p0_score_ci(p_hat, -1), "trials must be a natural number")
  expect_error(solve_p0_score_ci(p_hat, 1.2), "trials must be a natural number")
  # - z is numeric
  expect_error(solve_p0_score_ci(p_hat, trials, "R2D2"), "z must be numeric")

  # EXPECT EQUALS
  result <- solve_p0_score_ci(p_hat, trials, 1.96)
  expect_equal(result$p0, 0.7889839, tolerance = 1e-7)
  expect_equal(result$confidence_interval, c(0.5958436, 0.9821243), tolerance = 1e-7)
})
