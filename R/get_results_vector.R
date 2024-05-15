get_results_vector <- function(hypothesis, differences, vars = NULL, diff_method = 'wilcoxon', grp_a = NULL, grp_b = NULL, phi_0=0.5) {

  n = length(differences)
  valid_hypotheses <- c('decrease', 'increase', 'different')
  valid_diff_methods = c('wilcoxon', 't')

  # vars, grp_a and grp_b not null if 'different' in hypothesis
  # vars needs to be length n

  if (is.vector(differences) && !all(sapply(differences, function(x) is.numeric(x)))) { # differences numeric vector
    stop("differences must be a vector containing numeric values")
  }

  # diff_method one of 't' or 'wilcoxon'

  # if grp_a and grp_b not null then grp_a and grp_b column variables need to be the same
  # then vars needs to be a valid subset of grp_a column variables

  # if hypothesis is a string in valid_hypotheses,
  # then hypothesis_vector is a vector of length n of hypothesis
  if (is.character(hypothesis) && length(hypothesis) == 1) {
    hypothesis_vector <- switch(
      hypothesis[[1]],
      increase = rep('increase', n),
      decrease = rep('decrease', n),
      different = rep('different', n),
    )
  } else if (is.vector(hypothesis) && all(hypothesis %in% valid_hypotheses)) {
    hypothesis_vector <- hypothesis
  } else {
    stop("hypothesis must be either a string of length one or a vector of the following strings: 'increase', 'decrease', 'different'.")
  }

  # error handling and variable assignment
  # verifying equal length of hypothesis_vector, vars, and differences
  if (length(hypothesis_vector) != length(differences)){
    stop("differences, vars, and  hypothesis must be same length vectors, unless hypothesis is a string")
  }

  # creating results vector

  results <- numeric(n)# empty stack to eventually fill with return results (0s failure 1s success)

  # looping through hypothesis_vector and differences to verify success or failure
  # if 'different' then
  for (i in 1:n) {
    if (hypothesis_vector[i] == 'increase' && differences[i] > 0) {
      results[i] <- 1
    } else if (hypothesis_vector[i] == 'decrease' && differences[i] < 0) {
      results[i] <- 1
    } else if (hypothesis_vector[i] == 'different') {
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

  return(results)  # a vector of 0s and 1s
}


