solve_p0 <- function(p_hat, n, z = 1.96) {
  center <- (1 / (1 + (z^2 / n))) * (p_hat + (z^2 / (2 * n)))
  diff <- (z / (1 + (z^2 / n))) * ((p_hat * (1 - p_hat) / n) + (z^2 / (4 * (n^2)))) ^ 0.5
  lower <- center - diff
  upper <- center + diff
  return(list(point_estimate = center, confidence_interval = c(lower, upper)))
}
