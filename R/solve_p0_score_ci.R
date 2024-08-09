#' Calculate the Adjusted Proportion Estimate and Confidence Interval
#'
#' This function calculates the adjusted proportion estimate (p0) and the confidence interval for a given proportion estimate (p_hat) and sample size (n) using the score method.
#'
#' @param p_hat Numeric value. The proportion estimate. Must be between 0 and 1.
#' @param n Numeric value. The sample size. Must be a positive integer.
#' @param z Numeric value. The z-score for the desired confidence level. Default is 1.96 (approximately 95% confidence interval).
#'
#' @return A list with two elements:
#'   \item{p0}{The adjusted proportion estimate.}
#'   \item{confidence_interval}{A numeric vector of length 2 containing the lower and upper bounds of the confidence interval.}
#'
#' @examples
#' solve_p0_score_ci(p_hat = 9/10, n = 10)
#'
#' @export
solve_p0_score_ci <- function(p_hat, n, z = 1.96) {
  if (!is.numeric(p_hat)) {
    stop("p_hat must be numeric")
  }
  if (p_hat < 0 || p_hat > 1) {
    stop("p_hat must be on the interval (0,1)")
  }
  if (!is.numeric(n)) {
    stop("trials must be numeric.")
  }
  if (n <= 0 || floor(n) != n) {
    stop("trials must be a natural number")
  }
  if (!is.numeric(z)) {
    stop("z must be numeric")
  }
  center <- (1 / (1 + (z^2 / n))) * (p_hat + (z^2 / (2 * n)))
  diff <- (z / (1 + (z^2 / n))) * ((p_hat * (1 - p_hat) / n) + (z^2 / (4 * (n^2)))) ^ 0.5
  lower <- center - diff
  upper <- center + diff
  return(list(p0 = center, confidence_interval = c(lower, upper)))
}
