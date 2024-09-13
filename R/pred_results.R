#' Predictive Results Wrapper Function
#'
#' This function is a wrapper that conditionally handles filtering by group or
#' time, calculates the difference vector, and evaluates hypotheses to return a
#' list of results.
#'
#' @param dataset A data frame for research.
#' @param id The column that identifies unique subjects. This should be `NULL`
#' if `type` is 'group' and should not be `NULL` if `type` is 'prepost'.
#' @param vars The column variables of interest.
#' @param type The type of study. Valid values are 'group' for group-based data
#' and 'prepost' for pre-post data. Defaults to 'group'.
#' @param hypothesis A vector or string of valid hypotheses: 'increase', 'decrease',
#' or 'different'.
#' @param gtvar The column of interest to divide the groups (e.g., time or treatment).
#' @param grp_a The first subset of interest within the `gtvar` column
#' (e.g., 'pre' or 'control').
#' @param grp_b The second subset of interest within the `gtvar` column
#' (e.g., 'post' or 'treatment').
#' @param location The measure of central tendency to use for the difference
#' calculation. Valid options are 'median' or 'mean'. Defaults to 'median'.
#' @param diff_method The method to use for testing 'different' hypotheses. Valid
#' options are 'wilcoxon' or 't'. Defaults to 'wilcoxon'.
#' @param phi_0 The decision rule threshold for the p-value. If p-value < phi_0,
#' then there's sufficient evidence for a success for a difference. Defaults to 0.50.
#'
#' @return A list containing:
#' \describe{
#'   \item{results}{A vector of 0s and 1s indicating whether each hypothesis was correct.}
#'   \item{differences}{A vector of the differences between groups.}
#'   \item{variables}{The column variables used in the analysis.}
#' }
#'
#' @details This function performs error handling to ensure appropriate input values and
#'  types. It then filters the data based on the study type, calculates the difference vector,
#'  and evaluates the hypotheses using the specified method.
#'
#' @examples
#' data("group_data_example")
#' data("group_cog_data")
#' data("pre_post_data_example")
#' data("pre_post_fit")
#'
#' # simple group analysis
#' pred_results(dataset=group_data_example, vars=c('v1', 'v2'),
#' hypothesis=c("increase", "different"), gtvar="group", grp_a="placebo", grp_b="drug")
#'
#' # simple prepost analysis
#' pred_results(dataset=pre_post_data_example, id="ID", vars=c('v1', 'v2', 'v3'),
#' type="prepost", hypothesis="increase", gtvar="time", grp_a=0, grp_b=12)
#'
#' # simulated group analysis
#' pred_results(dataset=group_cog_data, vars=c('blind_moca_uncorrected', 'craft_verbatim'),
#' type="group", hypothesis="decrease", gtvar="group.factor", grp_a="Control", grp_b="ESKD")
#'
#' # simulated prepost analysis
#' pred_results(dataset=pre_post_fit, id="ID", vars=c('Flex_right', 'Flex_left'),
#' type="prepost", hypothesis="increase", gtvar="Time", grp_a=0, grp_b=1)
#' @export
pred_results <- function(dataset,
                        id=NULL,
                        vars,
                        type = "group",
                        hypothesis,
                        gtvar,
                        grp_a,
                        grp_b,
                        location = "median",
                        diff_method = "wilcoxon",
                        phi_0 = 0.50) {

  # error handling
  if (type == 'group' && !is.null(id)) {
    stop("ID should be NULL if type='group'")
  }

  if (type == 'prepost' && is.null(id)) {
    stop("ID can't be NULL if type = 'prepost'")
  }

  if (!gtvar %in% names(dataset)) {
    stop("gtvar must be a valid column name in dataset")
  }


  # assigns the filtered data frames to the vars df_a or df_b,
  # based on if type is "group" or "prepost"
  if (type == "group") {
    dfs = filter_by_group_var(dataset, gtvar, grp_a, grp_b, vars)
    df_a <- dfs$group_1
    df_b <- dfs$group_2
  } else if (type == "prepost") {
    dfs = filter_by_time_var(dataset, id, gtvar, grp_a, grp_b, vars)
    df_a <- dfs$pre
    df_b <- dfs$post
  }

  diff_vector <- create_difference_vector(df_a, df_b, location) # vector of the differences between groups
  # a vector of 1s (correctly predicted) and 0s (incorrectly predicted)
  results = get_results_vector(hypothesis, diff_vector, diff_method=diff_method, grp_a=df_a, grp_b=df_b) # grp_a/b don't need to be defined here, but it doesn't do anything if they are

  output <- list(results=results, differences=diff_vector, variables=vars) #col vars with desired for each one
  return(output)
}
