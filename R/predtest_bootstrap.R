predtest_bootstrap <- function(weights, results, nullphi = 0.50, alpha = 0.05, sims = 5000){

  test_stat <- weights%*%results
  ntests <- length(weights)


  # unique calculation for this function
  if (length(nullphi) == 1 | length(nullphi) == ntests)
  {
    boots <- matrix(NA,sims,1)
    for (g in 1:sims)
    {
      boots[g,] <- ifelse((rbinom(ntests,1,nullphi))%*%weights >= test_stat,1,0)
    }
    p_val <- mean(boots)

    return(p_val)

  } else{
    stop("nullphi needs to be either a single value or specified for every endpoint")
  }

}
