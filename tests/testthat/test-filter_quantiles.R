test_that("filter_quantiles filters and orders correctly", {
  # Mock data for centralities
  centralities <- data.frame(
    Betweenness = c(0.1, 0.2, 0.5, 0.8, 0.9),
    Closeness = c(0.3, 0.4, 0.6, 0.7, 0.95),
    Degree = c(0.05, 0.15, 0.45, 0.85, 0.99),
    row.names = c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE")
  )

  # Test filtering with quantile threshold of 0.8
  result <- filter_quantiles(centralities, quantiles = 0.8)

  # Check if the result is a data frame
  expect_true(is.data.frame(result))

  # Check the number of rows in the filtered result
  expect_equal(nrow(result), 1)
  # Check if the filtered genes meet the quantile condition
  quantile_values <- apply(centralities, 2, function(x) quantile(x, 0.8))
  genes_to_keep <- rownames(centralities)[
    apply(centralities, 1, function(x) all(x >= quantile_values))
  ]
  expect_equal(rownames(result), genes_to_keep)

  # Test ordering with 'Degree' as the orderBy column
  result_ordered <- filter_quantiles(centralities, quantiles = 0.5, orderBy = "Degree")

  # Check if the result is ordered by 'Degree' in descending order
  expect_equal(order(result_ordered$Degree, decreasing = TRUE), seq_len(nrow(result_ordered)))
})
