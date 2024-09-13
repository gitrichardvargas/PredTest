#' Create a Difference Vector
#'
#' This function calculates the difference between two groups of data based on a specified location measure (median or mean).
#'
#' @importFrom stats median
#' @param grp_1_data A data frame where all columns are numeric, representing the first group of data.
#' @param grp_2_data A data frame where all columns are numeric, representing the second group of data.
#' @param location A string specifying the location measure to use for calculating differences. Must be either 'median' or 'mean'.
#'
#' @return A numeric vector representing the differences between the second group's location measure and the first group's location measure for each column.
#'
#' @details The function checks if the specified location measure is valid ('median' or 'mean'). It also checks if both groups of data are numeric and if they have the same size and column variables. Based on the location measure, it calculates the differences and returns them as a numeric vector.
#'
#' @examples
#' df_1 <- data.frame(v1 = c(1, 2, 100),v2 = c(4, 5, 6))
#' df_2 <- data.frame(v1 = c(7, 6, 5),v2 = c(4, 3, 2))
#'
#' create_difference_vector(df_2, df_1, 'median')
#' create_difference_vector(df_2, df_1, 'mean')
#'
#' @export
create_difference_vector <- function(grp_1_data, grp_2_data, location='median') {
  possible_locations = c('median', 'mean')

  # Check if location is valid
  if (!location %in% possible_locations) {
    stop("Invalid location specified. Choose 'median' or 'mean'.")
  }

  # Check if both grp_1_data and grp_2_data are numeric
  if (!all(sapply(grp_1_data, is.numeric)) || !all(sapply(grp_2_data, is.numeric))) {
    stop("All columns in grp_1_data and grp_2_data must be numeric.")
  }

  # Check if grp_1_data and grp_2_data have the same number of columns
  if (ncol(grp_1_data) != ncol(grp_2_data)) {
    stop("grp_1_data and grp_2_data must have the same number of columns.")
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




