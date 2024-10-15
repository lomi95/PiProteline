test_that("emPAI_function works with a list of groups", {
  # Create mock data
  peptides_group1 <- matrix(c(5, 10, 15), nrow = 1)
  peptides_group2 <- matrix(c(8, 12, 20), nrow = 1)
  Pn_ideal <- c(10, 10, 10)
  list.groups_peptides <- list(Group1 = peptides_group1, Group2 = peptides_group2)

  # Compute emPAI
  result <- emPAI_function(list.groups_peptides, Pn_ideal)

  # Check that the result is a matrix
  expect_true(is.matrix(result))

  # Check that the dimensions are correct
  expect_equal(dim(result), c(3, 2))

  # Verify the values are non-negative
  expect_true(all(result >= 0))
})

test_that("emPAI_function works with a dataset", {
  # Create mock dataset (Samples x Proteins)
  dataset <- matrix(c(5, 10, 15, 8, 12, 20), nrow = 2)
  Pn_ideal <- c(10, 10, 10)

  # Compute emPAI
  result <- emPAI_function(dataset, Pn_ideal)

  # Check that the result is a vector
  expect_true(is.vector(result))

  # Check that the length is correct
  expect_equal(length(result), 3)

  # Verify the values are non-negative
  expect_true(all(result >= 0))
})
