#' Filter by Group Variable
#'
#' This function takes a data frame with two groups and splits them by a group identifier and specified column variables.
#'
#' @param df A data frame which must have a column to identify two different groups.
#' @param grp_var The column with the two groups, e.g., 'Treatment'.
#' @param grp_1 The first group identifier in the `grp_var` column, e.g., 'control'.
#' @param grp_2 The second group identifier in the `grp_var` column, e.g., 'drug_a'.
#' @param vars The column variables the researcher is interested in. The researcher can subset the columns instead of using all potential column variables.
#'
#' @return A list of two data frames that are subsets of the original
#' data frame, separated by their group status.
#'
#' @details This function checks if the input data frame, group variable,
#' and column variables are valid. It ensures that the specified groups exist
#' within the group variable column. The function then filters the data for each
#' group and returns a list containing the filtered data frames.
#'
#' @examples
#' # Load example data
#' data("group_data_example")
#'
#' # Use the function to filter by group
#' result <- filter_by_group_var(df=group_data_example, grp_var="group",
#' grp_1 ='placebo',grp_2 ='drug',vars=c("v1", "v2"))
#' print(result$group_1)
#' print(result$group_2)
#'
#' @export
filter_by_group_var = function(df, grp_var, grp_1, grp_2, vars) {
  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("The input 'df' must be a data frame.")
  }

  # Ensure grp_var is a valid column name in df
  if (!(grp_var %in% names(df))) {
    stop("The specified 'grp_var' is not a column in the data frame.")
  }

  # Check if vars are valid column names in df
  if (!all(vars %in% names(df))) {
    stop("All elements in 'vars' must be valid column names in the data frame.")
  }

  # Ensure grp_1 and grp_2 are valid values in the grp_var column
  if (!all(c(grp_1, grp_2) %in% df[[grp_var]])) {
    stop("Both 'grp_1' and 'grp_2' must be valid elements in the specified 'grp_var' column.")
  }

  # Filter data for each group
  grp_1_vars = df[df[[grp_var]] == grp_1, vars, drop = FALSE]
  grp_2_vars = df[df[[grp_var]] == grp_2, vars, drop = FALSE]


  # Place the data frames into a list and return it
  return(list(group_1 = grp_1_vars, group_2 = grp_2_vars))
}

