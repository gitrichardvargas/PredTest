#' Perform a prediction test based on specified parameters.
#'
#' This function conditionally calls the appropriate helper function based on the specified test type.
#'
#' @param weights_vector A vector of weights_vector.
#' @param results_vector A vector of results_vector.
#' @param test_type The type of test to perform ("exact", "approx", or "bootstrap").
#' @param phi_0 The null hypothesis value for the test.
#'
#' @return The p-value resulting from the prediction test.
#'
#' @examples
#' pred_test(big_weights, big_results_vector, test_type = "approx", phi_0 = 0.5)
#' pred_test(small_weights,small_results_vector, test_type="exact", phi_0=.5)
#' pred_test(small_weights_vector, small_results_vector, test_type="bootstrap", phi_0=.5)
#'
#' # Additional examples for other test types if needed...
#'
#' @export
#'
pred_test <- function(weights_vector, results_vector, test_type, phi_0 = 0.5, alpha = 0.05, sims = 5000) {

  # error handling to-dos

  # - remember that everything is a vector in R, so weights_vector and results_vector can be any natural number,
  # as long as they're the same len
  # - weights_vector and results_vector must be numeric
  # - all elements in weights_vector must be on the set (0,1)
  # - all elements in results_vector must be on the set {0,1}


  # - test_type must be one of "exact", "approx", "bootstrap"
  if (test_type != "exact" || test_type != "approx" || test_type != "bootstrap"){
    stop("Enter a valid argument for test_type: 'exact', 'approx', or 'bootstrap'.")
  }

  # - phi_0 and alpha must be on the set of (0,1)

  # phi_0
  if (!is.numeric(phi_0) || length(phi_0) != 1 || phi_0 > 1 || phi_0 < 0) {
    stop("p_hat must be a numeric value on the interval (0,1).")
  }

  # alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha > 1 || alpha < 0) {
    stop("p_hat must be a numeric value on the interval (0,1).")
  }

  # sims meet with Dr. Montgomery

  test_stat = weights_vector %*% results_vector
  phi_hat <- (1/sum(weights_vector)*test_stat)
  ci_list = solve_p0(phi_hat,length(weights_vector),1.96 ) # score confidence interval

  options <- c("exact", "approx", "bootstrap")


  if (test_type == "exact") {
    # contents of exact func
    ntests <- length(weights_vector)
    nperm <- 2^ntests
    perms <- as.matrix(expand.grid(rep(list(0:1), ntests)))
    values <- perms%*%as.matrix(weights_vector)
    rank <- as.data.frame(cbind(values,rank(values)))
    p_val <- dim(rank[rank$V2 >= rep(rank$V2[which(rank$V1 == as.numeric(test_stat))[1]], length(rank$V2)), ])[1] / nperm

  } else if (test_type == "approx") {
    # contents of approx func
    squares = weights_vector^2
    mu = phi_0 * sum(weights_vector)
    sigma = sqrt(phi_0 * (1 - phi_0) * sum(squares))
    z_score = (test_stat - mu) / sigma
    p_val = pnorm(z_score, lower.tail = FALSE)


  } else if (test_type == "bootstrap") {
    ntests <- length(weights_vector)


    # unique calculation for this function
    if (length(nullphi) == 1 | length(nullphi) == ntests)
    {
      boots <- matrix(NA,sims,1)
      for (g in 1:sims)
      {
        boots[g,] <- ifelse((rbinom(ntests,1,nullphi))%*%weights_vector >= test_stat,1,0)
      }
      p_val <- mean(boots)

      return(p_val)

    } else{
      stop("nullphi needs to be either a single value or specified for every endpoint")
    }
  }

  output <- c(list(
    num_correctly_predicted = sum(results_vector),
    p_value = p_val,
    test_stat = test_stat
  ),
  ci_list
  )

  return(output)
}
