test_that("flatten2CorrPPI works as expected", {
  # Create a mock correlation output using Hmisc::rcorr
  library(Hmisc)
  data <- matrix(c(0.1, 0.2, NA, 0.4, 0.5,
                   0.6, 0.7, 0.8, 0.9, NA,
                   1.0, 0.2, 0.3, 0.4, 0.5), ncol = 3)
  colnames(data) <- c("Gene1", "Gene2", "Gene3")
  corr_result <- rcorr(data)

  # Apply flatten2CorrPPI
  flattened_corr <- flatten2CorrPPI(corr_result)

  # Check the structure of the output
  expect_true(is.data.frame(flattened_corr))
  expect_true(all(c("row", "column", "cor", "p", "n", "cor_features") %in% colnames(flattened_corr)))

  # Check if NA values in correlation are replaced by 0
  expect_true(all(flattened_corr$cor[is.na(flattened_corr$cor)] == 0))

  # Check if NA values in p-values are replaced by 1
  expect_true(all(flattened_corr$p[is.na(flattened_corr$p)] == 1))

  # Verify that the number of rows in the output matches the expected flattened matrix size
  num_genes <- ncol(data)
  expected_rows <- (num_genes * (num_genes - 1)) / 2  # upper triangular without diagonal
  expect_equal(nrow(flattened_corr), expected_rows)
})
