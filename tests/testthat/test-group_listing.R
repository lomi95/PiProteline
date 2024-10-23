test_that("group_listing works with names_of_groups", {
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3",
                         "treatment_3", "control_4", "control_5", "treatment_4", "treatment_5")

  result <- group_listing(dataset, names_of_groups = c("control", "treatment"))

  expect_equal(length(result), 2)
  expect_equal(ncol(result[[1]]), 5)  # Control group
  expect_equal(ncol(result[[2]]), 5)  # Treatment group
})

test_that("group_listing works with pos.vectors_groups", {
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- paste0("row", 1:10)

  result <- group_listing(dataset, pos.vectors_groups = list(c(1, 3, 5), c(2, 4, 6)))

  expect_equal(length(result), 2)
  expect_equal(ncol(result[[1]]), 3)
  expect_equal(ncol(result[[2]]), 3)
})

test_that("group_listing gives error with no groups or just one group", {
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- paste0("col", 1:10)

  expect_error(group_listing(dataset),
               "Both `names_of_groups` and `pos.vectors_groups` are missing, with no default"
  )
  expect_error(group_listing(dataset, pos.vectors_groups = list(c(1, 11))),
               "`pos.vectors_groups` should be a list of length at least 2"
  )
  expect_error(group_listing(dataset, names_of_groups = "Group1"),
               "`names_of_groups` should be a character string of length at least 2"
  )
})

test_that("group_listing handles out of bounds indices", {
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- paste0("col", 1:10)

  expect_warning(
    result <- group_listing(dataset, pos.vectors_groups = list(c(1, 11), c(2,4))),
    "One or more elements are out of dataset dimension limits"
  )
  expect_equal(ncol(result[[1]]), 1)
})

test_that("group_listing stops if pos.vectors_groups is not a list", {
  dataset <- matrix(rnorm(100), ncol = 10)

  expect_error(group_listing(dataset, pos.vectors_groups = c(1, 2, 3)),
               "'pos.vectors_groups' should be a list")
})





test_that("group_listing works with names_of_groups", {
  # Create example dataset
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2",
                         "control_3", "treatment_3", "control_4", "control_5",
                         "treatment_4", "treatment_5")

  # Test function
  result <- group_listing(dataset, names_of_groups = c("control", "treatment"))

  # Check that the result is a list
  expect_true(is.list(result))

  # Check that the list contains two elements
  expect_equal(length(result), 2)

  # Verify that the column names match the specified groups
  expect_true(all(grepl("control", colnames(result[[1]]))))
  expect_true(all(grepl("treatment", colnames(result[[2]]))))
})

test_that("group_listing works with pos.vectors_groups", {
  # Create example dataset
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- paste0("Sample", 1:10)

  # Test function with pos.vectors_groups
  result <- group_listing(dataset, pos.vectors_groups = list(c(1, 3, 5), c(2, 4, 6)))

  # Check that the result is a list
  expect_true(is.list(result))

  # Check that the list contains two elements
  expect_equal(length(result), 2)

  # Verify that the columns match the specified indices
  expect_equal(ncol(result[[1]]), 3)
  expect_equal(ncol(result[[2]]), 3)
})

test_that("group_listing handles just_shared_genes correctly", {
  # Create example dataset
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2",
                         "control_3", "treatment_3", "control_4", "control_5",
                         "treatment_4", "treatment_5")
  rownames(dataset) <- paste0("Gene", 1:10)

  # Introduce some unique genes in each group
  dataset[1:3, 1:5] <- NA  # Genes 1-3 missing in control
  dataset[8:10, 6:10] <- NA # Genes 8-10 missing in treatment

  # Test function
  result <- group_listing(dataset, names_of_groups = c("control", "treatment"), just_shared_genes = TRUE)

  # Verify that only shared genes are present
  expect_equal(rownames(result[[1]]), rownames(result[[2]]))
})

test_that("group_listing handles freq parameter correctly", {
  # Create example dataset
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- paste0("Sample", 1:10)

  # Set some values to zero
  dataset[1:5, 1:3] <- 0

  # Test function with freq = 0.5
  result <- group_listing(dataset, pos.vectors_groups = list(1:5, 6:10), freq = 0.5)

  # Verify that only rows with more than 50% non-zero values are kept
  expect_true(all(rowSums(result[[1]] == 0) < 3))
})

test_that("group_listing gives warnings for overlapping columns", {
  # Create example dataset
  dataset <- matrix(rnorm(100), nrow = 10)
  colnames(dataset) <- paste0("Sample", 1:10)

  # Test function with overlapping groups
  expect_warning(group_listing(dataset, pos.vectors_groups = list(1:5, 3:7)),
                 "is/are selected for more than one group")
})
