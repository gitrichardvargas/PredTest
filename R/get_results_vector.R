
#' Get Results Vector
#'
#' This function receives user input on the hypothesis of the experiment results
#'  and informs the user if the hypotheses were correct. It can handle hypotheses
#'  of 'increase', 'decrease', or 'different', and performs appropriate
#'  statistical tests.
#'
#' @importFrom stats t.test wilcox.test
#' @param hypothesis A string or a vector of strings where the string or all
#' elements of the vector are in the set of \{'decrease', 'increase', 'different'\}.
#' If it’s a string, it will be converted to a vector of the same length as
#'  `differences` with all elements being the valid string.
#' @param differences A numeric vector representing the differences between
#' two groups.
#' @param diff_method A string specifying the method to use for testing
#' 'different' hypotheses. Valid options are 'wilcoxon' or 't'.
#' Defaults to 'wilcoxon'.
#' @param grp_a A data frame representing the first group for testing 'different'
#' hypotheses. This argument is only needed if 'different' is part of the hypothesis.
#' @param grp_b A data frame representing the second group for testing 'different'
#' hypotheses. This argument is only needed if 'different' is part of the hypothesis.
#' @param phi_0 A numeric value on the interval (0, 1) representing the decision rule
#' threshold for the p-value. Defaults to 0.5.
#'
#' @return A numeric vector of 0s and 1s. If the hypothesis was incorrect, a 0 is
#' returned. If the hypothesis was correct, a 1 is returned.
#'
#' @details The function checks if the input hypotheses are valid, and performs the
#' necessary statistical tests to determine if the hypotheses were correct. It
#' handles 'increase' and 'decrease' hypotheses by comparing differences, and
#' 'different' hypotheses by performing either a Wilcoxon signed-rank test
#' or a paired t-test.
#'
#' @examples
#' df_1 <- data.frame(v1 = c(1, 2, -100), v2 = c(40, 5, 6))
#' df_2 <- data.frame(v1 = c(7, 6, 5), v2 = c(4, 3, 2))
#'
#' differences <- create_difference_vector(df_2, df_1)
#'
#' # using singular increase
#' get_results_vector(hypothesis = 'increase', differences = differences)
#'
#' # using 'different' hypothesis and a pre post scenario
#' get_results_vector(hypothesis = c('increase', 'different'),
#' differences = differences, grp_a = df_1, grp_b = df_2, phi_0 = 0.05)
#' @export
get_results_vector <- function(hypothesis, differences, diff_method = 'wilcoxon', grp_a = NULL, grp_b = NULL, phi_0=0.5) {

  n = length(differences)
  valid_hypotheses <- c('decrease', 'increase', 'different')
  valid_diff_methods = c('wilcoxon', 't')

  # grp_a and grp_b not null if 'different' in hypothesis

  # -differences isn't a  numeric vector
  if (!is.numeric(differences) || any(!is.finite(differences))) {
    stop("differences must be a numeric vector")
  }

  # if hypothesis isn't a string of a valid hypothesis: increase, decrease, or different
  # or vector a of valid hypotheses: increase, decrease, or different
  if (!(is.character(hypothesis) && all(hypothesis %in% valid_hypotheses))) {
    stop("hypothesis must be either a valid string or a vector of valid strings: 'increase', 'decrease', 'different'")
  }

  # there are arguments that are set to NULL but 'different' is part of the hypothesis
  if ('different' %in% hypothesis && (is.null(grp_a) || is.null(grp_b))) {
    stop("Both grp_a and grp_b must be provided when 'different' is part of the hypothesis")
  }

  # if grp_a and grp_b are different sizes
  if (!is.null(grp_a) && !is.null(grp_b) && (nrow(grp_a) != nrow(grp_b) || ncol(grp_a) != ncol(grp_b))) {
    stop("grp_a and grp_b must have the same dimensions")
  }


  # columns in grp_a and grp_b should be numeric.
  if (any(sapply(grp_a, function(x) !is.numeric(x))) || any(sapply(grp_b, function(x) !is.numeric(x)))) {
    stop("All columns in grp_a and grp_b must be numeric")
  }

  # diff_method isn't equal to 'wilcoxon' or 't'
  if (!diff_method %in% c('wilcoxon', 't')) {
    stop("diff_method must be either 'wilcoxon' or 't'")
  }

  # phi_0 isn't numeric and on the interval (0,1)
  if (!is.numeric(phi_0) || phi_0 <= 0 || phi_0 >= 1) {
    stop("phi_0 must be a numeric value within the interval (0, 1)")
  }
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
  # verifying equal length of hypothesis_vectorand differences
  if (length(hypothesis_vector) != length(differences)){
    stop("differences and  hypothesis must be same length vectors, unless hypothesis is a string")
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
      if (diff_method == 't') {
        test_result <- t.test(grp_a[[i]], grp_b[[i]], paired = TRUE)
      } else if (diff_method == 'wilcoxon') {
        test_result <- wilcox.test(grp_a[[i]], grp_b[[i]], paired = TRUE)
      }
      results[i] <- if (test_result$p.value < phi_0) 1 else 0
    } else {
      results[i] <- 0
    }
  }

  return(results)  # a vector of 0s and 1s
}


