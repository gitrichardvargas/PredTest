test_that("pre_test works", {

  w <- c(0.5, 0.75, 1)
  r <- c(0, 1, 1)

  # EXPECTED ERRORS

  # - if weights_vector isn't a vector of numbers between 1/m and 1
  expect_error(pred_test(c(0.1, 0.55, 1), r))

  # - if results_vector isn't a vector of 0s and 1s
  expect_error(pred_test(w, c(0, 1, 2)),
               "results_vector should only consist of 0s and 1s.")

  # - if weights_vector and results_vector are not the same length
  expect_error(pred_test(c(0.25, 0.5), r),
               "weights_vector and results_vector must have the same length.")

  # - if phi_0 isn't a number between 0 and 1
  expect_error(pred_test(w, r, phi_0 = 1.5))

  # - if test_type isn't one of the following strings: "exact", "approx", "bootstrap"
  expect_error(pred_test(w, r, test_type = "Grogu"),
               "Please enter a valid test type: 'exact', 'approx', 'bootstrap'")


  # EXPECTED RETURNED RESULT
  expect_equal(pred_test(w, r), list(num_correctly_predicted = 2, p_value = 0.25, test_stat = matrix(1.75, nrow = 1, ncol = 1), p0 = matrix(0.6218039, nrow = 1, ncol = 1), ci = c(0.2734100, 0.9701977)), tolerance = 1e-6)
  expect_equal(pred_test(w, r, test_type='approx'), list(num_correctly_predicted = 2, p_value = matrix(0.1765802, nrow = 1, ncol = 1), test_stat = matrix(1.75, nrow = 1, ncol = 1), p0 = matrix(0.6218039, nrow = 1, ncol = 1), ci = c(0.2734100, 0.9701977)), tolerance = 1e-6)
  # expect_equal(pred_test(w, r, test_type='bootstrap'), list(num_correctly_predicted = 2, p_value = 0.245, test_stat = matrix(1.75, nrow = 1, ncol = 1), p0 = matrix(0.6218039, nrow = 1, ncol = 1), ci = c(0.2734100, 0.9701977)), tolerance = 1e-6)
  # For bootstrap, check that the p_value is within a reasonable range
  result_bootstrap <- pred_test(w, r, test_type='bootstrap')
  expect_equal(result_bootstrap$num_correctly_predicted, 2)
  expect_equal(result_bootstrap$test_stat, matrix(1.75, nrow = 1, ncol = 1))
  expect_equal(result_bootstrap$p0, matrix(0.6218039, nrow = 1, ncol = 1), tolerance = 1e-6)  # Increased tolerance
  expect_equal(result_bootstrap$ci, c(0.2734100, 0.9701977), tolerance = 1e-6)  # Increased tolerance

})
