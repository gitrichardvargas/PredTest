#' Create a Difference Vector
#'
#' This function calculates the difference between two groups of data based on a specified location measure (median or mean).
#'
#' @param grp_1_data A data frame where all columns are numeric, representing the first group of data.
#' @param grp_2_data A data frame where all columns are numeric, representing the second group of data.
#' @param location A string specifying the location measure to use for calculating differences. Must be either 'median' or 'mean'.
#'
#' @return A numeric vector representing the differences between the second group's location measure and the first group's location measure for each column.
#'
#' @details The function checks if the specified location measure is valid ('median' or 'mean'). It also checks if both groups of data are numeric and if they have the same size and column variables. Based on the location measure, it calculates the differences and returns them as a numeric vector.
#'
#' @examples
#' data("group_data_example")
#' data("group_cog_data")
#' data("pre_post_data_example")
#' data("pre_post_fit")
#'
#' # Example 1: Using group_data_example
#' grp_1_data <- subset(group_data_example, group == 'placebo', select = c('v1', 'v2'))
#' grp_2_data <- subset(group_data_example, group == 'drug', select = c('v1', 'v2'))
#' create_difference_vector(grp_1_data, grp_2_data, location='mean')
#'
#' # Example 2: Using pre_post_data_example
#' pre_data <- subset(pre_post_data_example, time == 0, select = c('v1', 'v2', 'v3'))
#' post_data <- subset(pre_post_data_example, time == 12, select = c('v1', 'v2', 'v3'))
#' create_difference_vector(pre_data, post_data, location='median')
#'
#' # Example 3: Using group_cog_data
#' control_data <- subset(group_cog_data, group.factor == 'Control', select = c('blind_moca_uncorrected', 'craft_verbatim'))
#' eskd_data <- subset(group_cog_data, group.factor == 'ESKD', select = c('blind_moca_uncorrected', 'craft_verbatim'))
#' create_difference_vector(control_data, eskd_data, location='mean')
#'
#' # Example 4: Using pre_post_fit
#' pre_fit <- subset(pre_post_fit, Time == 0, select = c('Flex_right', 'Flex_left'))
#' post_fit <- subset(pre_post_fit, Time == 1, select = c('Flex_right', 'Flex_left'))
#' create_difference_vector(pre_fit, post_fit, location='mean')

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

  # Check if grp_1_data and grp_2_data are the same size
  if (ncol(grp_1_data) != ncol(grp_2_data)) {
    stop("grp_1_data and grp_2_data must have the same number of rows")
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




