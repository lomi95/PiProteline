test_that("manova performs manova analysis correctly", {
  # Create mock dataset
  set.seed(123)
  dataset <- matrix(rnorm(100), nrow = 20)
  colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3")
  rownames(dataset) <- paste("gene", 1:20, sep = "_")

  # Perform manova
  result <- suppressWarnings(manova(dataset, names_of_groups = c("Control", "Treatment"), gene_column = 0))

  # Check that the result is a list
  expect_true(is.data.frame(result))

  # Check that the result contains expected components
  expect_true("GeneName" %in% names(result))
  expect_true("p.adj" %in% names(result))
  expect_true("DAve_Control_vs_Treatment" %in% names(result))
  expect_true("FC_Control_vs_Treatment" %in% names(result))
})

test_that("manova handles missing groups correctly", {
  # Create dataset where no groups match
  set.seed(123)
  dataset <- matrix(rnorm(100), nrow = 20)
  colnames(dataset) <- c("sample1", "sample2", "sample3", "sample4", "sample5")
  rownames(dataset) <- paste("gene", 1:20, sep = "_")

  # Expect error when no groups are found
  expect_error(manova(dataset, names_of_groups = c("control", "treatment"), gene_column = 0),
               regexp = "`names_of_groups` were not found in dataset colnames")

  expect_error(manova(dataset, pos_vectors_groups = list(c(1,5,9), c(2,123,13465)), gene_column = 0),
               "Attempt to select index of columns greater than number of columnns in dataset")
})

