test_that("Corr_Between_Groups handles valid input correctly", {
  # Create example data
  group1 <- matrix(rnorm(100), nrow = 10)
  group2 <- matrix(rnorm(100), nrow = 10)
  group3 <- matrix(rnorm(100), nrow = 10)
  data.grouped <- list(Group1 = group1, Group2 = group2, Group3 = group3)

  # Test function
  result <- Corr_Between_Groups(data.grouped, corr_groups = "spearman")

  # Check that the result is a list
  expect_true(is.list(result))

  # Check that the length of the result matches the number of combinations
  expect_equal(length(result), 3)

  # Verify that each element is a correlation matrix
  expect_true(all(sapply(result, function(x) "r" %in% names(x))))
})

test_that("Corr_Between_Groups assigns default names if none provided", {
  # Create example data without names
  group1 <- matrix(rnorm(100), nrow = 10)
  group2 <- matrix(rnorm(100), nrow = 10)
  data.grouped <- list(group1, group2)

  # Test function
  result <- Corr_Between_Groups(data.grouped, corr_groups = "pearson")

  # Check that default names have been assigned
  expect_equal(names(result), c("Group_1_vs_Group_2"))
})

test_that("Corr_Between_Groups returns an error for non-list input", {
  # Create example non-list data
  data.grouped <- matrix(rnorm(100), nrow = 10)

  # Test function
  expect_error(Corr_Between_Groups(data.grouped, corr_groups = "spearman"),
               "'data.grouped' must be a list")
})

test_that("Corr_Between_Groups handles different correlation types", {
  # Create example data
  group1 <- matrix(rnorm(100), nrow = 10)
  group2 <- matrix(rnorm(100), nrow = 10)
  data.grouped <- list(Group1 = group1, Group2 = group2)

  # Test spearman correlation
  result_spearman <- Corr_Between_Groups(data.grouped, corr_groups = "spearman")
  expect_true(is.list(result_spearman))

  # Test pearson correlation
  result_pearson <- Corr_Between_Groups(data.grouped, corr_groups = "pearson")
  expect_true(is.list(result_pearson))

})

test_that("Corr_Between_Groups returns correctly named list elements", {
  # Create example data
  group1 <- matrix(rnorm(100), nrow = 10)
  group2 <- matrix(rnorm(100), nrow = 10)
  group3 <- matrix(rnorm(100), nrow = 10)
  data.grouped <- list(GroupA = group1, GroupB = group2, GroupC = group3)

  # Test function
  result <- Corr_Between_Groups(data.grouped, corr_groups = "spearman")

  # Verify that names match expected combinations
  expect_equal(names(result), c("GroupA_vs_GroupB", "GroupA_vs_GroupC", "GroupB_vs_GroupC"))
})
