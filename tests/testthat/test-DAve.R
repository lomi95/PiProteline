test_that("DAve correctly computes the index for pairwise group comparisons", {
  # Create mock data
  group1 <- matrix(c(1, 2, 3, 4, 5, 8), nrow = 2)
  group2 <- matrix(c(3, 2, 1, 6, 5, 4), nrow = 2)
  group3 <- matrix(c(5, 4, 3, 2, 1, 0), nrow = 2)
  groups_list <- list(Group1 = group1, Group2 = group2, Group3 = group3)
  groups_list <- lapply(groups_list, function(x){
    rownames(x) <- c("gene1","gene2")
    colnames(x) <- paste0("sample_",1:3)
    return(x)
  })

  # Compute DAve
  result <- DAve(groups_list)

  # Check that the result is a matrix
  expect_true(is.data.frame(result))

  # Check the dimensions of the result
  expect_equal(dim(result), c(nrow(group1), 4))

  # Verify the names of the columns
  expected_colnames <- c("GeneName",
                         "Group1_vs_Group2",
                         "Group1_vs_Group3",
                         "Group2_vs_Group3")
  expect_equal(colnames(result), expected_colnames)

  # Test that the computed values are correct for a known input
  expected_values <- (rowMeans(groups_list$Group1) - rowMeans(groups_list$Group2)) /
    ((rowMeans(groups_list$Group1) + rowMeans(groups_list$Group2)) * 0.5)
  expect_equal(result[, "Group1_vs_Group2"], as.vector(expected_values))
})
