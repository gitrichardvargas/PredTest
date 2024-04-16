filter_by_group_var <- function(df, grp_var, grp_1, grp_2, vars){
  # Subset the data by group with only the column variables mentioned in ...
  grp_1_vars <- df[df[[grp_var]] == grp_1, vars]
  grp_2_vars <- df[df[[grp_var]] == grp_2, vars]
  # place the data frames into a list and return it
  return(list(grp_1_vars = grp_1_vars, grp_2_vars = grp_2_vars))
}
