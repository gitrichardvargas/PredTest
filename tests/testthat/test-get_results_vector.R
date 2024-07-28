

test_that("get_results_vector handles inputs and tests appropriately", {

  df_a = data.frame(
    v1 = c(1.5, 3.2, 5.8, 7.6),
    v2 = c(2.1, 4.5, 6.7, 8.9),
    v3 = c(0.5, 2.1, 3.8, 2.1)
  )

  df_b = data.frame(
    v1 = c(1.6, 3.1, 6.8, 7.4),
    v2 = c(3.5, 5.6, 9.8, 13.1),
    v3 = c(0.3, 1.1, 0.7, 1.7)
  )

  # note:v1 is very similar in both groups, there's increases in v1 and v2, but v3 decreases
  mean_differences = colMeans(df_b) - colMeans(df_a) #  0.075  2.450 -1.175

  # WRONG INPUTS:
  df_a_wrong_num_cols <- subset(df_a, select = -v1)
  df_strings <- data.frame(
    v1 = c("Adm. Akbar", "Luke Skywalker", "Kylo Ren", "Jawas"),
    v2 = c("Darth Vader", "Obi-Wan Kenobi", "Yoda", "Han Solo"),
    v3 = c("Leia Organa", "Chewbacca", "R2-D2", "C-3PO")
  )

  # EXPECT ERRORS

  # differences isn't a numeric vector
  expect_error(get_results_vector('increase', c("a", "b", "c")),
               "differences must be a numeric vector")

  # if hypothesis isn't a string of a valid hypothesis: increase, decrease, or different
  # or vector a of valid hypotheses: increase, decrease, or different
  expect_error(get_results_vector("invalid", mean_differences),
               "hypothesis must be either a valid string or a vector of valid strings: 'increase', 'decrease', 'different'")
  expect_error(get_results_vector(c("up", "down"), mean_differences),
               "hypothesis must be either a valid string or a vector of valid strings: 'increase', 'decrease', 'different'")

  # THROW ERROR CONT. BUT IF 'different' is either the string or
  # an element in the vector of hypothesis


  # if grp_a and grp_b are different sizes: eg. shouldn't work with df_a_wrong_num_cols and df_b
  expect_error(get_results_vector('increase', mean_differences, grp_a = df_a_wrong_num_cols, grp_b = df_b),
               "grp_a and grp_b must have the same dimensions")

  # columns in grp_a and grp_b should be numeric.
  expect_error(get_results_vector('increase', mean_differences, grp_a = df_strings, grp_b = df_b),
               "All columns in grp_a and grp_b must be numeric")

  # diff_method isn't equal to 'wilcoxon' or 't'
  expect_error(get_results_vector('different', mean_differences, diff_method = "invalid", grp_a = df_a, grp_b = df_b),
               "diff_method must be either 'wilcoxon' or 't'")

  # phi_0 isn't numeric and on the interval (0,1)
  expect_error(get_results_vector('different', mean_differences, diff_method = 't', phi_0 = 'wookie', grp_a = df_a, grp_b = df_b))


  ##################################

  # EXPECT EQUAL TO'S

  # simple increase
  expect_equal(c(1,1,0), get_results_vector('increase', mean_differences))

  # simple decrease
  expect_equal(c(0,0,1), get_results_vector('decrease', mean_differences))

  # vector of increase + decrease
  inc_dec_vector = c('increase', 'increase', 'decrease')
  expect_equal(c(1,1,1), get_results_vector(inc_dec_vector, mean_differences))

  # EXPECT EQUAL TO'S CONT. BUT IF 'different' IS OR IS PART OF HYPOTHESIS

  # simple different with t test
  expect_equal(c(0,1,1), get_results_vector(hypothesis = 'different',
                                            differences = mean_differences,
                                            diff_method = 't',
                                            grp_a = df_a,
                                            grp_b = df_b )) # assumes phi_0 = 0.5
  # simple different with Wilcoxon Signed Rank Test
  expect_equal(c(0,1,1), get_results_vector(hypothesis = 'different',
                                            differences = mean_differences,
                                            grp_a = df_a,
                                            grp_b = df_b )) # assumes phi_0 = 0.5, diff_method = wilcoxon

  # vector of all hypothesis options w/ t test
  inc_dec_diff_vect = c('increase', 'decrease', 'different')
  expect_equal(c(1,0,1), get_results_vector(hypothesis = inc_dec_diff_vect,
                                            differences = mean_differences,
                                            diff_method = 't',
                                            grp_a = df_a,
                                            grp_b = df_b )) # assumes phi_0 = 0.5

  # vector of all hypothesis options w/  Wilcoxon Signed Rank Test
  inc_dec_diff_vect = c('increase', 'decrease', 'different')
  expect_equal(c(1,0,1), get_results_vector(hypothesis = inc_dec_diff_vect,
                                            differences = mean_differences,
                                            grp_a = df_a,
                                            grp_b = df_b )) # assumes phi_0 = 0.5

})


