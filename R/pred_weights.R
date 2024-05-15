predweights <- function(dataset, variables, type = "group", id=NULL, pre=NULL, post=NULL, gtvar, corr_method = "pearson") {
  # throw warning if there's missing data
  if (type == "group") {
    # read the article again
    # learn what's happening here
    endpoints <- dataset[, variables]
    samplec <- cor(endpoints, method = corr_method)
  } else if (type == "prepost") {
    dfs <- filter_by_time_var(dataset, id, gtvar, pre, post, variables)
    df_a <- dfs$pre_vars
    df_b <- dfs$post_vars
    pre_post_difference <- df_b - df_a
    samplec <- cor(pre_post_difference, method = corr_method)
  }

  weights <- 1/rowSums(samplec^2)
  outweight <- list(weights)
  return(outweight)
}
