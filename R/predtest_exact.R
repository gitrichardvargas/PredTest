predtest_exact = function(weights, results, phi_0=.5) {

  test_stat = weights %*% results
  ntests <- length(weights)
  nperm <- 2^ntests
  perms <- as.matrix(expand.grid(rep(list(0:1), ntests)))
  values <- perms%*%as.matrix(weights)
  rank <- as.data.frame(cbind(values,rank(values)))
  p_val <- dim(rank[rank$V2 >= rep(rank$V2[which(rank$V1 == as.numeric(test_stat))[1]], length(rank$V2)), ])[1] / nperm

  return(p_val)

}
