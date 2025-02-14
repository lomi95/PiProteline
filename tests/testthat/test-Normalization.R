test_that("normalization handles ln transformation correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "ln")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Verify that values are correctly log-transformed
  expect_equal(result$Protein1[1], log(10 + 1))
})

test_that("normalization handles Z-score normalization correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "Znorm")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Verify that Z-score mean is approximately zero
  expect_equal(mean(result$Protein1), 0, tolerance = 1e-8)
})

test_that("normalization handles Min-Max scaling correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "MinMax")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Check that all values are between 0 and 1
  expect_true(all(result >= 0 & result <= 1))
})

test_that("normalization handles Robust scaling correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "Robust")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Verify that scaling is based on median and IQR
  expect_equal(median(result$Protein1), 0, tolerance = 1e-8)
})

test_that("normalization handles Unit vector scaling correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "UnitVector")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Verify that the norm of each column is 1
  expect_equal(sqrt(sum(result$Protein1^2)), 1, tolerance = 1e-8)
})

test_that("normalization handles Total Signal normalization correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "TotSigNorm")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Verify that each column sums to 1
  expect_equal(sum(result$Protein1), 1, tolerance = 1e-8)
})

test_that("normalization handles Maximum Signal normalization correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "MaxSigNorm")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Verify that the maximum value in each column is 1
  expect_equal(max(result$Protein1), 1)
})

test_that("normalization handles Row Sigma normalization correctly", {
  data <- data.frame(
    Protein1 = c(10, 20, 30),
    Protein2 = c(5, 15, 25),
    Protein3 = c(2, 8, 18)
  )
  result <- normalization(data, norm_type = "RowSigmaNorm")

  # Verify dimensions
  expect_equal(dim(result), dim(data))

  # Verify that the values are scaled by mean + 3 * sd
  expect_equal(result$Protein1[1], 10 / (mean(data$Protein1) + 3 * sd(data$Protein1)), tolerance = 1e-8)
})

