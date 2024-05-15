
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
