# test_that("Gtest computes the G statistic correctly", {
#   # Create mock data
#   group1 <- c(1, 2, 3, 4, 5)
#   group2 <- c(2, 3, 4, 5, 6)
#   group3 <- c(3, 4, 5, 6, 7)
#   list.groups <- list(Group1 = group1, Group2 = group2, Group3 = group3)
#
#   # Compute G values
#   result <- Gtest(list.groups)
#
#   # Check that the result is a list
#   expect_true(is.list(result))
#
#   # Check that the result has the correct length (3 pairwise comparisons for 3 groups)
#   expect_equal(length(result), 3)
#
#   # Test the G value for the comparison between Group1 and Group2
#   (2 * group1 * log(group1 / ((group1 + group2) / 2)) + 2 * group2 * log(group2 / ((group1 + group2) / 2))) * sign(group1 - group2)
#   expected_value <- (2 * group1 * log(group1 / ((group1 + group2) / 2)) +
#                        2 * group2 * log(group2 / ((group1 + group2) / 2))) * sign(group1 - group2)
#   expect_equal(result[[1]], expected_value)
# })
#
# test_that("Gtest handles groups with zeros and warnings are triggered for negative or zero values", {
#   # Create mock data with zeros
#   group1 <- c(0, 2, 3, 4, 5)
#   group2 <- c(2, 3, 4, 5, 6)
#   list.groups <- list(Group1 = group1, Group2 = group2)
#
#   # Test if the function triggers warnings for zero or negative values
#   expect_warning(result <- Gtest(list.groups), "NaNs produced")
#
#   # Check that the result is still returned as a list
#   expect_true(is.list(result))
# })
#
# test_that("Gtest throws an error when list.groups has less than two groups", {
#   # Create a mock dataset with only one group
#   group1 <- c(1, 2, 3)
#   list.groups <- list(Group1 = group1)
#
#   # The function should throw an error if there's only one group
#   expect_error(Gtest(list.groups), "not enough observations")
# })
