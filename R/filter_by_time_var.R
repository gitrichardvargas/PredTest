filter_by_time_var = function(df, id, time_var, pre, post, vars){
  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("The input 'df' must be a data frame.")
  }

  # Check if vars are valid column names in df
  if (!all(vars %in% names(df))) {
    stop("All elements in 'vars' must be valid column names in the data frame.")
  }

  # Ensure time_var is a valid column name in df
  if (!(time_var %in% names(df))) {
    stop("The specified 'time_var' is not a column in the data frame.")
  }


  # Ensure pre and post are valid values in the time_var column
  if (!all(c(pre, post) %in% df[[time_var]])) {
    stop("Both 'pre' and 'post' must be valid elements in the specified 'time_var' column.")
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

