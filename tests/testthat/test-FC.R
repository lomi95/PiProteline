test_that("FC computes the correct pairwise fold changes", {
  # Create mock data
  group1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  group2 <- matrix(c(3, 2, 1, 6, 5, 4), nrow = 2)
  group3 <- matrix(c(5, 4, 3, 2, 1, 0), nrow = 2)
  groups_list <- list(Group1 = group1, Group2 = group2, Group3 = group3)
  groups_list <- lapply(groups_list, function(x){
    rownames(x) <- c("gene1","gene2")
    colnames(x) <- paste0("sample_",1:3)
    return(x)
  })

  # Compute FC
  result <- FC(groups_list)

  # Check that the result is a matrix
  expect_true(is.data.frame(result))

  # Check that the dimensions are correct (2 rows, 6 pairwise comparisons)
  expect_equal(dim(result), c(2, 4))

  # Verify the names of the columns
  expected_colnames <- c("GeneName",
                         "Group1_vs_Group2",
                         "Group1_vs_Group3",
                         "Group2_vs_Group3")
  expect_equal(colnames(result), expected_colnames)

  # Check that the calculated fold changes are correct for known input
  expected_values <- log2(rowMeans(group1) / rowMeans(group2))
  expect_equal(result[, "Group1_vs_Group2"], expected_values)
})

test_that("FC handles edge cases with zero values", {
  # Create mock data with zeros
  group1 <- matrix(c(1, 2, 0), nrow = 3)
  group2 <- matrix(c(0, 0, 0), nrow = 3)
  groups_list <- list(Group1 = group1, Group2 = group2)
  groups_list <- lapply(groups_list, function(x){
    rownames(x) <- c("gene1","gene2","gene3")
    colnames(x) <- paste0("sample_1")
    return(x)
  })

  result <- FC(groups_list)

  # Check that the result still returns a matrix
  expect_true(is.data.frame(result))
})

