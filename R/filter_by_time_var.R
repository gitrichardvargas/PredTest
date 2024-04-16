filter_by_time_var <- function(df, id, time_var, pre, post, vars){
  # Separates the pre and post results into separate dfs
  grp_pre <- df[df[time_var] == pre, ]
  grp_post <- df[df[time_var] == post, ]
  # Place in ascending order, so that the ids are in the same row
  # COULD USE SOME ERROR HANDLING HERE TO MAKE SURE THE IDS ARE IDENTICAL
  sorted_grp_pre <- grp_pre[order(grp_pre[[id]]), ]
  sorted_grp_post <- grp_post[order(grp_post[[id]]), ]
  # Filters such that only the chosen variables in vars are present
  # in each data frame
  pre_vars <- sorted_grp_pre[vars]
  post_vars <- sorted_grp_post[vars]
  return(list(pre_vars = pre_vars, post_vars = post_vars))
}
