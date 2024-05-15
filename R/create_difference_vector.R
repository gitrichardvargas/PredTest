create_difference_vector <- function(grp_1_data, grp_2_data, location='median') {
  possible_locations = c('median', 'mean')

  # Check if location is valid
  if (!location %in% possible_locations) {
    stop("Invalid location specified. Choose 'median' or 'mean'.")
  }

  # Check if both inputs are of the same type and either data frames or matrices
  if (!(is.data.frame(grp_1_data) && is.data.frame(grp_2_data)) &&
      !(is.matrix(grp_1_data) && is.matrix(grp_2_data))) {
    stop("Both grp_1_data and grp_2_data must be of the same type and either data frames or matrices")
  }

  # Check if both grp_1_data and grp_2_data are numeric
  if (!all(sapply(grp_1_data, is.numeric)) || !all(sapply(grp_2_data, is.numeric))) {
    stop("All columns in grp_1_data and grp_2_data must be numeric.")
  }

  # Check if grp_1_data and grp_2_data are the same size
  if (nrow(grp_1_data) != nrow(grp_2_data) || ncol(grp_1_data) != ncol(grp_2_data)) {
    stop("grp_1_data and grp_2_data must have the same dimensions.")
  }

  # Check if the column variables are the same
  if (!identical(names(grp_1_data), names(grp_2_data))) {
    stop("Column variables in grp_1_data and grp_2_data must be the same.")
  }

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

