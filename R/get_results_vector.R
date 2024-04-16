get_results_vector <- function(hypothesis, vars, differences, diff_method = 'wilcoxon', grp_a = NULL, grp_b = NULL, phi_0=0.5) {
  # error handling

  if (!all(sapply(differences, function(x) is.numeric(x)))) {
    stop("differences must only contain numeric values")
  }

  valid_hypotheses <- c('decrease', 'increase', 'different')

  # if hypothesis is a string it becomes a vector of length(vars) of a valid hypothesis
  if (is.character(hypothesis) && length(hypothesis) == 1) {
    hypothesis_vector <- switch(
      hypothesis[[1]],  # Access the first element of the hypothesis vector
      increase = rep('increase', length(vars)),
      decrease = rep('decrease', length(vars)),
      different = rep('different', length(vars)),
      stop("Invalid hypothesis. Choose from a string or vector containing: 'decrease', 'increase', 'different'")
    )
  } else if (is.vector(hypothesis) && all(hypothesis %in% valid_hypotheses)) {
    hypothesis_vector <- hypothesis
  } else {
    stop("Invalid hypothesis. It must be either a string or a vector of the strings: 'increase', 'decrease', 'different'.")
  }
  # confirm same sized vectors for differences, vars, and hypothesis (no longer be a string)
  if (length(differences) != length(hypothesis_vector)) stop("differences, vars, and  hypothesis must be same length vectors, unless hypothesis is a string")
  if (length(differences) != length(vars)) stop("differences, vars, and  hypothesis must be same length vectors, unless hypothesis is a string")

  # error handling cases of handling 'different' as a hypothesis

  # empty stack to eventually return results
  results <- numeric(length(differences))

  # Declare n as the length of vars
  n <- length(vars)

  for (i in 1:n) {
    if (hypothesis_vector[i] == 'increase' && differences[i] > 0) {
      results[i] <- 1
    } else if (hypothesis_vector[i] == 'decrease' && differences[i] < 0) {
      results[i] <- 1
    } else if (hypothesis_vector[i] == 'different') {
      # might need to create a new function to subset the variable columns to do the tests on
      if (diff_method == 'wilcoxon') {
        # do wilcox.test on vars[i] in grp_a and grp_b
        test_result <- wilcox.test(grp_a[[vars[i]]], grp_b[[vars[i]]], paired = TRUE, alternative = "two.sided")
      } else if (diff_method == 't') {
        # do t.test on vars[i] in grp_a and grp_b
        test_result <- t.test(grp_a[[vars[i]]], grp_b[[vars[i]]], paired = TRUE)
      }
      if (test_result$p.value < phi_0){
        results[i] <- 1
      }
    } else {
      results[i] <- 0
    }
  }

  return(results)
}
