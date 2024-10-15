test_that("FC computes the correct pairwise fold changes", {
  # Create mock data
  group1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  group2 <- matrix(c(3, 2, 1, 6, 5, 4), nrow = 2)
  group3 <- matrix(c(5, 4, 3, 2, 1, 0), nrow = 2)
  list.groups <- list(Group1 = group1, Group2 = group2, Group3 = group3)

  # Compute FC
  result <- FC(list.groups)

  # Check that the result is a matrix
  expect_true(is.matrix(result))

  # Check that the dimensions are correct (2 rows, 6 pairwise comparisons)
  expect_equal(dim(result), c(2, 6))

  # Verify the names of the columns
  expected_colnames <- c("Group1_vs_Group2", "Group2_vs_Group1",
                         "Group1_vs_Group3", "Group3_vs_Group1",
                         "Group2_vs_Group3", "Group3_vs_Group2")
  expect_equal(colnames(result), expected_colnames)

  # Check that the calculated fold changes are correct for known input
  expected_values <- -log2(rowMeans(group1) / rowMeans(group2))
  expect_equal(result[, "Group1_vs_Group2"], expected_values)
})

test_that("FC handles edge cases with zero values", {
  # Create mock data with zeros
  group1 <- matrix(c(1, 2, 0), nrow = 3)
  group2 <- matrix(c(2, 2, 2), nrow = 3)
  list.groups <- list(Group1 = group1, Group2 = group2)

  result <- FC(list.groups)

  # Check that the result still returns a matrix
  expect_true(is.matrix(result))
})

