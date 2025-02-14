test_that("centralities_over_quantiles handles valid input correctly", {
  # Create example centralities data
  group1 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
  rownames(group1) <- paste0("Gene", 1:100)
  group2 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
  rownames(group2) <- paste0("Gene", 1:100)

  centralities <- list(Group1 = group1, Group2 = group2)

  # Test function
  result <- centralities_over_quantiles(centralities, merge_all = TRUE, quantile_critical_nodes = 0.9)

  # Verify that the result is a matrix
  expect_true(is.matrix(result))

  # Check that the dimensions match the original number of genes and metrics
  expect_equal(dim(result), c(100, 4))
})

test_that("centralities_over_quantiles handles merge_all = FALSE correctly", {
  # Create example centralities data with some different rownames
  group1 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
  rownames(group1) <- paste0("Gene", 1:100)
  group2 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
  rownames(group2) <- paste0("Gene", 51:150)

  centralities <- list(Group1 = group1, Group2 = group2)

  # Test function with merge_all = FALSE
  result <- centralities_over_quantiles(centralities, merge_all = FALSE, quantile_critical_nodes = 0.9)

  # Verify that only common genes are included in the result
  expect_equal(nrow(result), 50)
})

test_that("centralities_over_quantiles returns correct critical nodes", {
  # Create example centralities data
  set.seed(123)
  group1 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
  rownames(group1) <- paste0("Gene", 1:100)
  group2 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
  rownames(group2) <- paste0("Gene", 1:100)

  centralities <- list(Group1 = group1, Group2 = group2)

  # Test function for the top 90th percentile
  result <- centralities_over_quantiles(centralities, merge_all = TRUE, quantile_critical_nodes = 0.9)

  # Check that approximately 10% of values are TRUE
  critical_fraction <- sum(result) / length(result)
  expect_true(critical_fraction > 0.08 && critical_fraction < 0.12)
})

test_that("centralities_over_quantiles assigns correct names to output columns", {
  # Create example centralities data
  group1 <- data.frame(Degree = rnorm(10), Closeness = rnorm(10))
  rownames(group1) <- paste0("Gene", 1:10)
  group2 <- data.frame(Degree = rnorm(10), Closeness = rnorm(10))
  rownames(group2) <- paste0("Gene", 1:10)

  centralities <- list(Group1 = group1, Group2 = group2)

  # Test function
  result <- centralities_over_quantiles(centralities, merge_all = TRUE, quantile_critical_nodes = 0.9)

  # Verify that column names contain the group identifiers
  expect_true(all(grepl("_Group1|_Group2", colnames(result))))
})

test_that("centralities_over_quantiles works with a single group", {
  # Create example centralities data
  group1 <- data.frame(Degree = rnorm(10), Closeness = rnorm(10))
  rownames(group1) <- paste0("Gene", 1:10)

  centralities <- list(Group1 = group1)

  # Test function
  result <- centralities_over_quantiles(centralities, merge_all = TRUE, quantile_critical_nodes = 0.9)

  # Verify that the result has the correct dimensions
  expect_equal(dim(result), c(10, 2))
})
