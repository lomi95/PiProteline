
test_that("DCI computes the correct pairwise comparisons", {
  # Create mock data
  group1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  group2 <- matrix(c(3, 2, 1, 6, 5, 4), nrow = 2)
  group3 <- matrix(c(5, 4, 3, 2, 1, 0), nrow = 2)
  list.groups <- list(Group1 = group1, Group2 = group2, Group3 = group3)

  # Compute DCI
  result <- DCI(list.groups)

  # Check that the result is a matrix
  expect_true(is.matrix(result))

  # Check that the dimensions are correct
  expect_equal(dim(result), c(2, 6))  # 2 rows, 6 pairwise comparisons

  # Verify the names of the columns
  expected_colnames <- c("Group1_vs_Group2", "Group2_vs_Group1",
                         "Group1_vs_Group3", "Group3_vs_Group1",
                         "Group2_vs_Group3", "Group3_vs_Group2")
  expect_equal(colnames(result), expected_colnames)

  # Check that the calculated DCI values are correct for known input
  expected_values <- (rowMeans(group1) + rowMeans(group2)) *
    (rowMeans(group1) - rowMeans(group2)) / 2
  expect_equal(result[, "Group1_vs_Group2"], expected_values)
})

test_that("DCI handles edge cases with empty groups", {
  # Create mock data with an empty group
  group1 <- matrix(c(1, 2, 3), nrow = 3)
  group2 <- matrix(c(2, 2, 2), nrow = 3)
  list.groups <- list(Group1 = group1, Group2 = group2, Group3 = matrix(numeric(0), nrow = 3))

  # Expect no errors when running DCI
  expect_silent(DCI(list.groups))
})
