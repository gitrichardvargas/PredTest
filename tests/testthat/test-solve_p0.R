# test valid argument entries for solve_p0


# test valid argument entries for solve_p0
test_that("solve_p0 function works correctly", {
  # Test valid input
  result <- solve_p0(0.5, 100, 1.96)
  expect_equal(length(result), 2)
  expect_true(is.numeric(result$point_estimate))
  expect_true(length(result$confidence_interval) == 2)
  expect_true(all(is.numeric(result$confidence_interval)))

  # Test invalid p_hat (outside the interval [0,1])
  tryCatch({
    solve_p0(1.5, 100, 1.96)
  }, error = function(e) {
    expect_true(grepl("p_hat must be a numeric value on the interval \\(0,1\\)", e$message))
  })

  # tests  invalid n (not a positive integer)
  expect_error(solve_p0(0.5, 2.5, 1.96), "n must be a positive integer.")

  # tests invalid z (not numeric)
  expect_error(solve_p0(0.5, 100, "abc"), "z must be a numeric value.")

  # expected return value
  result <- solve_p0(0.9, 10)
  expect_equal(result$point_estimate, 0.7889839, tolerance = 1e-6)
  expect_equal(result$confidence_interval[1], 0.5958436, tolerance = 1e-6)
  expect_equal(result$confidence_interval[2], 0.9821243, tolerance = 1e-6)
})
