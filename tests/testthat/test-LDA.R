test_that("LDA performs LDA analysis correctly", {
  # Create mock dataset
  set.seed(123)
  dataset <- matrix(rnorm(100), nrow = 20)
  colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3")
  rownames(dataset) <- paste("gene", 1:20, sep = "_")

  # Perform LDA
  result <- LDA(dataset, names_of_groups = c("control", "treatment"), gene_column = 0)

  # Check that the result is a list
  expect_true(is.list(result))

  # Check that the result contains expected components
  expect_true("dataset.LDA" %in% names(result))
  expect_true("features_p.values" %in% names(result))
  expect_true("features_updown" %in% names(result))
  expect_true("mds.plot" %in% names(result))
  expect_true("VolcanoPlots" %in% names(result))
})

test_that("LDA handles missing groups correctly", {
  # Create dataset where no groups match
  set.seed(123)
  dataset <- matrix(rnorm(100), nrow = 20)
  colnames(dataset) <- c("sample1", "sample2", "sample3", "sample4", "sample5")
  rownames(dataset) <- paste("gene", 1:20, sep = "_")

  # Expect error when no groups are found
  expect_error(LDA(dataset, names_of_groups = c("control", "treatment"), gene_column = 0),
               regexp = "`names_of_groups` were not found in dataset colnames")

  expect_error(LDA(dataset, pos.vectors_groups = list(c(1,5,9), c(2,123,13465)), gene_column = 0),
               "Attempt to select index of columns greater than number of columnns in dataset")
})

test_that("LDA performs correct filtering and significant feature selection", {
  # Create mock dataset
  set.seed(123)
  dataset <- matrix(rnorm(100), nrow = 20)
  colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3")
  rownames(dataset) <- paste("gene", 1:20, sep = "_")

  # Perform LDA
  result <- LDA(dataset, names_of_groups = c("control", "treatment"), gene_column = 0)

  # Check that the result contains significant features
  expect_true(nrow(result$features_p.values) > 0)
})
