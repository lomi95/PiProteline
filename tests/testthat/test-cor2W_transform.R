test_that("cor2W_transform correctly transforms weights", {
  # Example flattened correlation data frame
  flatt_corr <- data.frame(
    cor = c(0.8, 0.5, -0.3, 0.9),
    p.adj = c(0.05, 0.02, 1, 0.8)
  )
  significance_corr <- 0.05

  result <- cor2W_transform(flatt_corr, significance_corr)

  # Check if the result includes a 'weights' column
  expect_true("weights" %in% colnames(result))

  # Ensure that weights haven't been modified for significant p-values
  expect_equal(result$weights[1], result$cor[1])

  # Ensure that weights is lower for non-significant p-values
  expect_lt(result$weights[4], result$cor[4])
})
