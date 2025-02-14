
test_that("DCI computes the correct pairwise comparisons", {
  # Create mock data
  group1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  group2 <- matrix(c(5, 2, 6, 6, 5, 4), nrow = 2)
  group3 <- matrix(c(5, 8, 3, 9, 1, 0), nrow = 2)
  groups_list <- list(Group1 = group1, Group2 = group2, Group3 = group3)

  groups_list <- lapply(groups_list, function(x){
    rownames(x) <- c("gene1","gene2")
    colnames(x) <- paste0("sample_",1:3)
    return(x)
  })

  # Compute DCI
  result <- DCI(groups_list)

  # Check that the result is a matrix
  expect_true(is.data.frame(result))

  # Check that the dimensions are correct
  expect_equal(dim(result), c(2, 4))  # 2 rows, 6 pairwise comparisons

  # Verify the names of the columns
  expected_colnames <- c("GeneName",
                         "Group1_vs_Group2",
                         "Group1_vs_Group3",
                         "Group2_vs_Group3")
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
  groups_list <- list(Group1 = group1, Group2 = group2, Group3 = matrix(numeric(0), nrow = 3))

  groups_list <- lapply(groups_list, function(x){
    rownames(x) <- c("gene1","gene2","gene3")
    return(x)
  })
  # Expect no errors when running DCI
  expect_silent(DCI(groups_list))
})
