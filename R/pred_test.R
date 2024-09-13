
#' Predictive Test Function
#'
#' This function performs statistical tests to determine the predictive power of a results set weighted by a corresponding vector of weights. It offers various methods to conduct the test, allowing flexibility depending on the data characteristics and analysis requirements.
#' @importFrom stats pnorm rbinom
#' @param weights_vector A numeric vector where each element represents the weight for a corresponding result in the results vector. Each value must be on the interval \eqn{[1/m, 1]}, where \eqn{m = 1/length(weights_vector)}.
#' @param results_vector A numeric vector of test results where each element is in the set \{0, 1\}, representing the binary outcome of each prediction.
#' @param test_type A character string specifying the type of statistical test to perform. The valid options are 'exact', 'approx', or 'bootstrap'.
#' @param phi_0 A numeric value on the interval (0, 1) representing the null hypothesis value against which the test results are compared.
#' @param sims A natural number that specifies the number of simulations to perform when the bootstrap method is chosen. This parameter allows control over the robustness of the bootstrap approximation.
#'
#' @return A list containing:
#' \describe{
#'   \item{num_correctly_predicted}{The number of results correctly predicted as per the specified criteria.}
#'   \item{p_value}{The p-value resulting from the test, indicating the probability of observing the test results under the null hypothesis.}
#'   \item{test_stat}{The test statistic calculated based on the weights and results.}
#'   \item{p0}{The estimated proportion derived from the weights and results.}
#'   \item{ci}{A confidence interval for the estimated proportion derived from the weights and results using the Wilson score method.}
#' }
#'
#' @details This function performs error handling to ensure appropriate input values and types. It then calculates the test statistic and evaluates the p-value based on the specified test type.
#'
#' @examples
#' # Example weights and results vectors
#' weights_vector <- c(1/3, 0.5, 1)
#' results_vector <- c(0, 1, 1)
#'
#' # Exact test
#' result_exact <- pred_test(weights_vector, results_vector, test_type = 'exact')
#' result_exact
#'
#' # Approximate test
#' result_approx <- pred_test(weights_vector, results_vector, test_type = 'approx')
#' result_approx
#'
#' # Bootstrap test
#' result_bootstrap <- pred_test(weights_vector, results_vector, test_type = 'bootstrap')
#' result_bootstrap
#'
#' @export
pred_test <- function(weights_vector, results_vector, test_type='exact', phi_0 = 0.5, sims = 5000) {
  if (!is.numeric(weights_vector) || !is.numeric(results_vector)) {
    stop("weights_vector and results_vector must be numeric.")
  }

  if (length(weights_vector) != length(results_vector)) {
    stop("weights_vector and results_vector must have the same length.")
  }

  if (!all(results_vector %in% c(0, 1))) {
    stop("results_vector should only consist of 0s and 1s.")
  }

  m <- length(weights_vector)
  if (any(weights_vector < (1 / m) | weights_vector > 1)) {
    stop("All elements in weights_vector should be on the interval [1/length(weights_vector), 1].")
  }

  valid_test <- c('exact', 'approx', 'bootstrap')

  if (!(test_type %in% valid_test)) {
    stop("Please enter a valid test type: 'exact', 'approx', 'bootstrap'")
  }

  if (!is.numeric(phi_0) || length(phi_0) != 1 || phi_0 >= 1 || phi_0 <= 0) {
    stop("phi_0 must be a numeric value on the interval (0,1).")
  }

  test_stat <- weights_vector %*% results_vector
  phi_hat <- (1/sum(weights_vector)*test_stat)
  score_list <- solve_p0_score_ci(phi_hat, length(weights_vector), 1.96 )

  if (test_type == "exact") {
    ntests <- length(weights_vector)
    nperm <- 2^ntests
    perms <- as.matrix(expand.grid(rep(list(0:1), ntests)))
    values <- perms %*% as.matrix(weights_vector)
    rank <- as.data.frame(cbind(values, rank(values)))
    p_val <- dim(rank[rank$V2 >= rep(rank$V2[which(rank$V1 == as.numeric(test_stat))[1]], length(rank$V2)), ])[1] / nperm

  } else if (test_type == "approx") {
    squares <- weights_vector^2
    mu <- phi_0 * sum(weights_vector)
    sigma <- sqrt(phi_0 * (1 - phi_0) * sum(squares))
    z_score <- (test_stat - mu) / sigma
    p_val <- pnorm(z_score, lower.tail = FALSE)

  } else if (test_type == "bootstrap") {
    ntests <- length(weights_vector)

    if (length(phi_0) == 1 | length(phi_0) == ntests) {
      boots <- matrix(NA, sims, 1)
      for (g in 1:sims) {
        boots[g,] <- ifelse((rbinom(ntests, 1, phi_0)) %*% weights_vector >= test_stat, 1, 0)
      }
      p_val <- mean(boots)

    } else {
      stop("phi_0 needs to be either a single value or specified for every endpoint")
    }
  }

  output <- list(
    num_correctly_predicted = sum(results_vector),
    p_value = p_val,
    test_stat = matrix(test_stat, nrow = 1, ncol = 1),
    p0 = matrix(score_list$p0, nrow = 1, ncol = 1),
    ci = score_list$confidence_interval
  )

  return(output)
}
