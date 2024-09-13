#' Filter by Time Variable
#'
#' This function creates two groups based on their ID and separates them by a time variable.
#' Each ID will be present in each subset exactly once. The subsets will contain the same variables specified by the user.
#'
#' @param df A data frame which must have a column to identify two different groups by ID.
#' @param id The column variable that will contain all of the IDs that show up twice in the data set.
#' @param time_var The column variable that designates the time of the data, e.g., it may be a 0 if
#' data was collected on the first day of treatment and a 6 if data is collected six days later.
#' @param pre The value in the `time_var` column that indicates the 'pre' time point.
#' @param post The value in the `time_var` column that indicates the 'post' time point.
#' @param vars The column variables the researcher is interested in. The researcher can subset the
#' columns instead of using all potential column variables.
#'
#' @return A list of two data frames that are subsets of the original data frame, separated
#' by their `time_var` status. The data frames will have the same size in rows.
#'
#' @details This function checks if the input data frame, ID, time variable, and column
#' variablesare valid. It ensures that the specified pre and post values exist within the
#' time variable column. The function then filters the data for each time point, orders the
#' IDs, and returns a list containing the filtered data frames.
#'
#' @examples
#' # Load example data
#' data("pre_post_data_example")
#'
#' # Use the function to filter by time variable
#' result <- filter_by_time_var(pre_post_data_example, id = "ID",
#' time_var = "time", pre = 0, post = 12, vars = c("v1", "v2"))
#' print(result$pre)
#' print(result$post)
#'
#' @export
filter_by_time_var = function(df, id, time_var, pre, post, vars){
  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("The input 'df' must be a data frame.")
  }

  # Ensure id is a valid column name in df
  if (!(id %in% names(df))) {
    stop("The specified 'id' is not a column in the data frame.")
  }

  # Ensure time_var is a valid column name in df
  if (!(time_var %in% names(df))) {
    stop("The specified 'time_var' is not a column in the data frame.")
  }

  # Ensure pre and post are valid values in the time_var column
  if (!all(c(pre, post) %in% df[[time_var]])) {
    stop("Both 'pre' and 'post' must be valid elements in the specified 'time_var' column.")
  }

  # Check if vars are valid column names in df
  if (!all(vars %in% names(df))) {
    stop("All elements in 'vars' must be valid column names in the data frame.")
  }

  # Separates the pre and post results into separate dfs
  grp_pre = df[df[time_var] == pre, ]
  grp_post = df[df[time_var] == post, ]
  # Place in ascending order, so that the ids are in the same row
  sorted_grp_pre = grp_pre[order(grp_pre[[id]]), ]
  sorted_grp_post = grp_post[order(grp_post[[id]]), ]
  # Filters such that only the chosen variables in vars are present
  # in each data frame
  pre_df = sorted_grp_pre[vars]
  post_df = sorted_grp_post[vars]

  # Check that both subsets have the same number of rows
  if (nrow(pre_df) != nrow(post_df)) {
    stop("pre and post should have an equal size")
  }

  return(list(pre = pre_df, post = post_df))
}
