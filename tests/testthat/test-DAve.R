test_that("DAve correctly computes the index for pairwise group comparisons", {
  # Create mock data
  group1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  group2 <- matrix(c(3, 2, 1, 6, 5, 4), nrow = 2)
  group3 <- matrix(c(5, 4, 3, 2, 1, 0), nrow = 2)
  list.groups <- list(Group1 = group1, Group2 = group2, Group3 = group3)
  list.groups <- lapply(list.groups, function(x){
    rownames(x) <- c("gene1","gene2")
    colnames(x) <- paste0("sample_",1:3)
    return(x)
  })

  # Compute DAve
  result <- DAve(list.groups)

  # Check that the result is a matrix
  expect_true(is.matrix(result))

  # Check the dimensions of the result
  expect_equal(dim(result), c(nrow(group1), 6))

  # Verify the names of the columns
  expected_colnames <- c("Group1_vs_Group2", "Group2_vs_Group1",
                         "Group1_vs_Group3", "Group3_vs_Group1",
                         "Group2_vs_Group3", "Group3_vs_Group2")
  expect_equal(colnames(result), expected_colnames)

  # Test that the computed values are correct for a known input
  expected_values <- (rowMeans(list.groups$Group1) - rowMeans(list.groups$Group2)) /
    ((rowMeans(list.groups$Group1) + rowMeans(list.groups$Group2)) / 0.5)
  expect_equal(result[, "Group1_vs_Group2"], expected_values)
})
