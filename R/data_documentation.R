#' Adjusted Example Dataset
#'
#' A simulated dataset demonstrating group differences in three variables. This dataset includes two groups and one covariate, `sex`.
#'
#' @format A data frame with 20 rows and 5 variables:
#' \describe{
#'   \item{group}{A binary factor indicating group membership: `0` for control and `1` for treatment.}
#'   \item{sex}{A binary factor indicating sex: `0` for male and `1` for female.}
#'   \item{v1}{A numeric vector representing the first variable.}
#'   \item{v2}{A numeric vector representing the second variable.}
#'   \item{v3}{A numeric vector representing the third variable.}
#' }
#' @examples
#' data(adjusted_example)
#' head(adjusted_example)
"adjusted_example"

#' Group Data Example
#'
#' A simulated dataset showing group differences across four variables. The dataset is divided into two groups: `placebo` and `drug`.
#'
#' @format A data frame with 30 rows and 5 variables:
#' \describe{
#'   \item{group}{A factor indicating group membership: `placebo` or `drug`.}
#'   \item{v1}{A numeric vector representing the first variable.}
#'   \item{v2}{A numeric vector representing the second variable.}
#'   \item{v3}{A numeric vector representing the third variable.}
#'   \item{v4}{A numeric vector representing the fourth variable.}
#' }
#' @examples
#' data(group_data_example)
#' head(group_data_example)
"group_data_example"

#' Pre-Post Data Example
#'
#' A simulated dataset showing measurements before and after an intervention. Each subject has multiple measurements over time.
#'
#' @format A data frame with 30 rows and 6 variables:
#' \describe{
#'   \item{ID}{A unique identifier for each subject.}
#'   \item{time}{A numeric variable indicating time points, `0` for pre-intervention and `12` for post-intervention.}
#'   \item{v1}{A numeric vector representing the first variable.}
#'   \item{v2}{A numeric vector representing the second variable.}
#'   \item{v3}{A numeric vector representing the third variable.}
#'   \item{v4}{A numeric vector representing the fourth variable.}
#' }
#' @examples
#' data(pre_post_data_example)
#' head(pre_post_data_example)
"pre_post_data_example"

#' Group Cognitive Data
#'
#' A dataset representing cognitive scores for control and treatment groups, with various cognitive and demographic variables.
#'
#' @format A data frame with 20 rows and 20 variables:
#' \describe{
#'   \item{group.factor}{A factor indicating group membership: `Control` or `ESKD` (End-Stage Kidney Disease).}
#'   \item{mean_suv}{A numeric vector representing the mean SUV (Standard Uptake Value).}
#'   \item{blind_moca_uncorrected}{A numeric vector representing uncorrected MOCA (Montreal Cognitive Assessment) scores.}
#'   \item{craft_verbatim}{A numeric vector representing scores on the Craft Verbatim memory test.}
#'   \item{craft_delay_verbatim}{A numeric vector representing delayed scores on the Craft Verbatim memory test.}
#'   \item{number_span_forward}{A numeric vector representing forward number span scores.}
#'   \item{number_span_backward}{A numeric vector representing backward number span scores.}
#'   \item{fluency_f_words_correct}{A numeric vector representing the number of correct F words in a verbal fluency test.}
#'   \item{oral_trail_part_a}{A numeric vector representing scores on the oral trail making test part A.}
#'   \item{oral_trail_part_b}{A numeric vector representing scores on the oral trail making test part B.}
#'   \item{fluency_animals}{A numeric vector representing the number of animal names listed in a verbal fluency test.}
#'   \item{fluency_vegetables}{A numeric vector representing the number of vegetable names listed in a verbal fluency test.}
#'   \item{verbal_naming_no_cue}{A numeric vector representing scores on a verbal naming test without cues.}
#'   \item{age}{A numeric vector representing the age of each subject.}
#' }
#' @examples
#' data(group_cog_data)
#' head(group_cog_data)
"group_cog_data"

#' Pre-Post Fitness Data
#'
#' A dataset showing physical and cognitive performance measures before and after a fitness intervention.
#'
#' @format A data frame with 20 rows and 12 variables:
#' \describe{
#'   \item{ID}{A unique identifier for each subject.}
#'   \item{Time}{A numeric variable indicating time points, `0` for pre-intervention and `1` for post-intervention.}
#'   \item{Sex}{A numeric variable indicating sex: `0` for male and `1` for female.}
#'   \item{Age}{A numeric variable representing the age of each subject.}
#'   \item{COPM_p}{A numeric vector representing performance scores on the Canadian Occupational Performance Measure (COPM).}
#'   \item{COPM_s}{A numeric vector representing satisfaction scores on the COPM.}
#'   \item{A1_work}{A numeric vector representing work capacity scores at time A1.}
#'   \item{A2_work}{A numeric vector representing work capacity scores at time A2.}
#'   \item{Grip_dom}{A numeric vector representing grip strength of the dominant hand.}
#'   \item{Grip_ndom}{A numeric vector representing grip strength of the non-dominant hand.}
#'   \item{Flex_right}{A numeric vector representing right arm flexibility.}
#'   \item{Flex_left}{A numeric vector representing left arm flexibility.}
#' }
#' @examples
#' data(pre_post_fit)
#' head(pre_post_fit)
"pre_post_fit"
