test_that("LDA_pairwise works with valid input", {
  # Create an example dataset
  set.seed(123)
  data <- data.frame(
    gene = paste0("gene_", 1:10),
    matrix(rnorm(90), ncol = 9)
  )
  colnames(data)[2:10] <- paste0("group", rep(1:3, each = 3), "_", rep(1:3, times = 3))

  # Run the function with valid groups
  result <- suppressMessages(LDA_pairwise(data, names_of_groups = c("Group1", "Group2"), gene_column = 1))

  # Check that the result is a list
  expect_true(is.list(result))

  # Check that the list name contains the compared groups
  expect_equal(names(result), "Group1_vs_Group2")
})

test_that("LDA_pairwise performs all group combinations", {
  # Create an example dataset
  set.seed(123)
  data <- data.frame(
    gene = paste0("gene_", 1:10),
    matrix(rnorm(90), ncol = 9)
  )
  colnames(data)[2:10] <- paste0("group", rep(1:3, each = 3), "_", rep(1:3, times = 3))

  # Run the function with multiple groups
  result <- suppressMessages(LDA_pairwise(dataset = data, names_of_groups = c("Group1", "Group2", "Group3"), gene_column = 1))

  # Check that there are 3 combinations (Group1 vs Group2, Group1 vs Group3, Group2 vs Group3)
  expect_equal(length(result), 3)

  # Check that the combination names are correct
  expect_equal(names(result), c("Group1_vs_Group2", "Group1_vs_Group3", "Group2_vs_Group3"))
})


test_that("LDA_pairwise accepts additional parameters", {
  # Create an example dataset
  set.seed(123)
  data <- data.frame(
    gene = paste0("gene_", 1:10),
    matrix(rnorm(90), ncol = 9)
  )
  colnames(data)[2:10] <- paste0("group", rep(1:3, each = 3), "_", rep(1:3, times = 3))

  # Check that the function accepts additional parameters
  result <- suppressMessages(LDA_pairwise(data, names_of_groups = c("Group1", "Group2"), gene_column = 1, boundFC = c(0.5, 0.5)))

  # Check that the result is a list
  expect_true(is.list(result))
})
