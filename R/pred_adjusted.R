#' Adjusted Predictions Based on Group Comparisons
#'
#'
#' This function calculates adjusted predictions for variables of
#' interest, taking into account covariates and group comparisons. It then returns
#' whether the results align with the hypothesized direction of effects.
#'
#' @importFrom stats cov2cor
#'
#' @param dataset A data frame containing the data to be analyzed.
#' @param hypothesis A string or vector of strings containing either 'increase'
#' or 'decrease', indicating the expected direction of the effect.
#' @param vars A vector of variable names in the dataset that are the outcomes
#' of interest. These must be numeric columns.
#' @param covariates A vector of covariates to include in the model. These must
#' be numeric columns in the dataset.
#' @param group The name of the grouping variable in the dataset. This must be
#' a column in the dataset and should not overlap with `vars` or `covariates`.
#' @param ref The reference category within the group variable. This must be a
#' value present in the group column.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{results}{A vector indicating whether each hypothesis was correct
#'   (1 for correct, 0 for incorrect).}
#'   \item{weights}{A vector of weights corresponding to each variable in `vars`,
#'   calculated from the correlation matrix.}
#' }
#'
#' @examples
#' data("group_cog_data")
#' data("adjusted_example")
#'
#' # simple example
#' pred_adjusted(adjusted_example, c("decrease", "increase"),
#' c('v1', 'v2'), 'sex', "group", 0)
#'
#' # simulated example
#' pred_adjusted(dataset = group_cog_data, hypothesis = "decrease",
#' vars = c('craft_verbatim', 'fluency_f_words_correct'),
#' covariates = c('number_span_forward', 'number_span_backward'),
#' group = "group.factor", ref = "Control")
#'
#' @export
pred_adjusted <- function(dataset, hypothesis, vars, covariates, group, ref){

  # - vars must be valid column variables in dataset
  if (!all(vars %in% colnames(dataset))) {
    stop("All elements in vars must be column variables in the dataset")
  }
  # - covariates must be valid column variables in dataset
  if (!all(covariates %in% colnames(dataset))) {
    stop("All elements in covariates must be column variables in the dataset")
  }

  # - group must be a single column variable in dataset and not in vars
  if (!group %in% colnames(dataset)) {
    stop("group must be a column variable in the dataset")
  }
  # - ref must be a row variable in group
  if (!(ref %in% unique(dataset[, group]))) {
    stop("Reference value must be present in the group variable")
  }
  # vars, covariates, and group must be disjoint subsets that are col vars in dataset
  if (group %in% vars) {
    stop("group must be disjoint from vars")
  }
  if (group %in% covariates) {
    stop("group must be disjoint from covariates")
  }

  # - rows in vars must be numeric
  if (any(sapply(dataset[, vars], function(x) !is.numeric(x)))) {
    stop("All elements in vars must be numeric")
  }

  # - covariates must be numeric
  if (any(sapply(dataset[, covariates], function(x) !is.numeric(x)))) {
    stop("All covariates must be numeric")
  }

  nends <- length(vars)
  n <- dim(dataset)[1]
  levels <- unique(factor(dataset[,group]))

  if (missing(ref)){
    stop("Please specify a reference category.")
  } else{
    dataset[,group] <- ifelse(dataset[,group] == ref, 0, 1)

  }

  if(length(levels)>2){
    stop("There are more than two groups.")
  }

  valid = c('increase', 'decrease')
  Xdesign <- matrix(NA,n, 1+length(covariates)+1 )
  Xdesign[,1] <- 1
  Xdesign[,2:(1+length(covariates))] <-  as.matrix(dataset[,covariates])
  Xdesign[,1+length(covariates)+1] <- as.matrix(dataset[,group])

  # Response matrix
  Ymat <- as.matrix(dataset[,vars])

  # Beta hat matrix
  Betahat <- solve(t(Xdesign)%*%Xdesign)%*%t(Xdesign)%*%Ymat

  # Sample covariance matrix:
  sigmahat <- (t(Ymat)%*%Ymat - t(Betahat)%*%t(Xdesign)%*%Ymat)/(n-length(covariates)-1 )

  # Convert to correlation matrix
  rhohat <- cov2cor(sigmahat)

  differences <- Betahat[dim(Betahat)[1],]
  if (length(hypothesis) == 1 && hypothesis[1] %in% valid) {
    if (hypothesis[1] == 'increase') {
      results <- ifelse(differences > 0, 1, 0)
    } else if (hypothesis[1] == 'decrease') {
      results <- ifelse(differences < 0, 1, 0)
    }
  } else if (length(hypothesis) > 1) {
    if (length(hypothesis) != length(differences)) {
      stop("If hypothesis is greater than one then it must be the same length as the number of vars")
    }
    results <- ifelse(hypothesis == 'increase', ifelse(differences > 0, 1, 0), ifelse(hypothesis == 'decrease', ifelse(differences < 0, 1, 0), NA))
    if (any(is.na(results))) {
      stop(paste("Invalid hypothesis value:", paste(hypothesis[is.na(results)], collapse = ", ")))
    }
  } else {
    stop("Enter a valid hypothesis: a string of 'increase', 'decrease' or a vector of strings containing them.")
  }

  weights <- 1/rowSums(rhohat^2)

  outlist <- list(results = results, weights = weights)
  return(outlist)
}


