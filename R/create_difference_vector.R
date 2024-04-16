create_difference_vector <- function(grp_1_data, grp_2_data, location='median'){
  if (location == 'mean') {
    grp_1_means <- colMeans(grp_1_data)
    grp_2_means <- colMeans(grp_2_data)
    differences <- grp_2_means - grp_1_means
    return(unname(as.vector(differences)))
  } else if (location == 'median') {
    grp_1_medians <- apply(grp_1_data, 2, median)
    grp_2_medians <- apply(grp_2_data, 2, median)
    differences <- grp_2_medians - grp_1_medians
    return(unname(as.vector(differences)))
  }
}
