# using base R to solve for p0 for a score confidence interval
solve_p0 <- function(p_hat, n, z = 1.96) {
  # Error handling for n
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n %% 1 != 0) {
    stop("n must be a positive integer.")
  }

  # Error handling for z
  if (!is.numeric(z) || length(z) != 1) {
    stop("z must be a numeric value.")
  }

  # Error handling for p_hat
  if (!is.numeric(p_hat) || length(p_hat) != 1 || p_hat > 1 || p_hat < 0) {
    stop("p_hat must be a numeric value on the interval (0,1).")
  }

  center <- (1 / (1 + (z^2 / n))) * (p_hat + (z^2 / (2 * n)))
  diff <- (z / (1 + (z^2 / n))) * ((p_hat * (1 - p_hat) / n) + (z^2 / (4 * (n^2)))) ^ 0.5
  lower <- center - diff
  upper <- center + diff
  return(list(point_estimate = center, confidence_interval = c(lower, upper)))
}


