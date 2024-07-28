#' Calculate Predictive Weights
#'
#' This function calculates predictive weights by computing the inverse square sum of a correlation matrix derived from the specified variables. In 'group' analysis, it directly uses the variables for correlation. In 'prepost' analysis, it calculates the difference between two time points before correlation.
#'
#' @param dataset A data frame containing the dataset to be analyzed.
#' @param vars A vector of strings specifying the names of the variables to be used in the correlation analysis.
#' @param gtvar The name of the categorical variable used to identify groups in 'prepost' type analysis.
#' @param type The type of analysis. Valid values are 'group' for group-based correlation analysis or 'prepost' for pre-post analysis. Defaults to 'group'.
#' @param id The variable in the dataset that uniquely identifies subjects in a 'prepost' analysis. This should not be `NULL` if `type` is 'prepost'.
#' @param pre Specifies the baseline time point for 'prepost' analysis.
#' @param post Specifies the follow-up time point for 'prepost' analysis.
#' @param corr_method The method of correlation. Valid options are 'pearson', 'kendall', or 'spearman'. Defaults to 'pearson'.
#'
#' @return A numeric vector of predictive weights for each variable analyzed.
#'
#' @details This function performs error handling to ensure appropriate input values and types. It calculates the correlation matrix for the specified variables and then computes the predictive weights as the inverse square sum of the correlation matrix.
#'
#' @examples
#' # Load example data
#' data("group_data_example")
#' data("pre_post_data_example")
#'
#' # Example usage for group-based data
#' weights_group <- pred_weights(
#'   dataset = group_data_example,
#'   vars = c("v1", "v2", "v3"),
#'   gtvar = "group",
#'   type = "group"
#' )
#' print(weights_group)
#'
#' # Example usage for pre-post data
#' weights_prepost <- pred_weights(
#'   dataset = pre_post_data_example,
#'   vars = c("v1", "v2", "v3"),
#'   gtvar = "time",
#'   type = "prepost",
#'   id = "ID",
#'   pre = 0,
#'   post = 12
#' )
#' print(weights_prepost)
#'
#' @export
pred_weights <- function(dataset, vars, gtvar, type = "group", id=NULL, pre=NULL, post=NULL, corr_method = "pearson") {
  # error handling
  available_types = c("group", "prepost")
  corr_methods = c("pearson", "kendall", "spearman")
  if (!(type %in% available_types)) {
    stop("type arugment must be 'group' or 'prepost'")
  }
  if (!(corr_method %in% corr_methods)) {
    stop("corr_method must be one of: 'pearson', 'kendall', or 'spearman'")
  }

  if (!all(vars %in% names(dataset))) {
    stop("All elements in 'vars' must be valid column names in the data frame.")
  }


  if (type == "group") {
    endpoints <- dataset[, vars]
    samplec <- cor(endpoints, method = corr_method)
  } else if (type == "prepost") {
    dfs <- filter_by_time_var(dataset, id, gtvar, pre, post, vars)
    df_a <- dfs$pre
    df_b <- dfs$post
    pre_post_difference <- df_b - df_a
    samplec <- cor(pre_post_difference, method = corr_method)
  }

  weights <- 1/rowSums(samplec^2)
  return(weights)
}
