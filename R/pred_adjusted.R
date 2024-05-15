predadjusted <- function(dataset, variables, covariates, group, ref){

  nends <- length(variables)
  n <- dim(dataset)[1]
  levels <- unique(factor(dataset[,group]))

  if (missing(ref)){
    dataset[,group] <- ifelse(dataset[,group] == levels[1], 0, 1)
  } else{
    dataset[,group] <- ifelse(dataset[,group] == ref, 0, 1)

  }


  if(length(levels)>2){
    stop("There are more than two groups.")
  }

  results <- matrix(NA,1,nends)

  # Building the design matrix
  # Number of columns is 1(Intercept) + number of covariates + variables for group/pre-post
  Xdesign <- matrix(NA,n, 1+length(covariates)+1 )
  Xdesign[,1] <- 1
  Xdesign[,2:(1+length(covariates))] <-  as.matrix(dataset[,covariates])
  Xdesign[,1+length(covariates)+1] <- as.matrix(dataset[,group])

  # Response matrix
  Ymat <- as.matrix(dataset[,variables])

  # Beta hat matrix
  Betahat <- solve(t(Xdesign)%*%Xdesign)%*%t(Xdesign)%*%Ymat

  # Sample covariance matrix:
  sigmahat <- (t(Ymat)%*%Ymat - t(Betahat)%*%t(Xdesign)%*%Ymat)/(n-length(covariates)-1 )

  # Convert to correlation matrix
  rhohat <- cov2cor(sigmahat)

  results[1,] <- Betahat[dim(Betahat)[1],]
  colnames(results) <- c(variables)

  differences <- results
  weights <- 1/rowSums(rhohat^2)



  results <- as.vector(results)
  weights <- as.vector(weights)
  outlist <- list(results, weights)
  return(outlist)
}
