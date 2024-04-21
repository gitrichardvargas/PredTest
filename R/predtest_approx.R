predtest_approx = function(weights, results,phi_0=.5){

  test_stat = weights %*% results
  sum_of_weights = sum(weights)
  squares = weights^2
  mu = phi_0 * sum_of_weights
  sigma = sqrt(phi_0 * (1 - phi_0) * sum(squares))
  z_score = (test_stat - mu) / sigma
  p_val = pnorm(z_score, lower.tail = FALSE)

  return(p_val)
}
