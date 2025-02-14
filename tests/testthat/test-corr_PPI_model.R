test_that("corr_PPI_model doesn't work with less than 4 observation", {
  # Create a mock dataset
  dataset <- data.frame(
    Gene1 = c(0.5, NA, 0.3),
    Gene2 = c(0.4, 0.6, 0),
    Gene3 = c(NA, 0.1, 0.2),
    row.names = c("Sample1", "Sample2", "Sample3")
  )

  # Mock gene identifiers
  genes_id <- c("Gene1", "Gene2", "Gene3")

  mock_interactome <- igraph::make_full_graph(3)
  igraph::V(mock_interactome)$name <- genes_id
  gcorr <- suppressWarnings(corr_PPI_model(
    dataset = dataset,
    genes_id = genes_id,
    corr_test = "spearman",
    significance_corr = 0.05,
    g_interactome = mock_interactome,
    tax_ID = 9606,
    na_as_zero = TRUE,
    zero_as_na = FALSE,
    compute_weights = TRUE
  ))
  # Test the function with the mock data
  expect_equal(all(is.na(edge.attributes(gcorr)$w)),T)

})

test_that("corr_PPI_model correctly builds network models with new mock dataset", {
  dataset <- data.frame(
    Gene1 = c(0.5, NA, 0.3, 0.6, 0.9),
    Gene2 = c(0.4, 0.6, 0, 0.8, 0.4),
    Gene3 = c(NA, 0.1, 0.2, 0.6, 0.9),
    Gene4 = c(0.7, 0.1, 0.2, 0.4, 0.1),
    row.names = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5")
  )

  genes_id <- c("Gene1", "Gene2", "Gene3", "Gene4")


  # Mock interactome graph using igraph
  mock_interactome <- igraph::make_full_graph(4)
  igraph::V(mock_interactome)$name <- genes_id

  # Mock rbioapi functions to avoid external calls
  mock_rba_string_interactions_network <- mockery::mock(
    data.frame(protein1 = c("Gene1", "Gene2", "Gene2"), protein2 = c("Gene4", "Gene3", "Gene4"))
  )
  mockery::stub(corr_PPI_model, "rbioapi::rba_string_interactions_network",
                mock_rba_string_interactions_network)

  # Test the function with the mock data
  result <- corr_PPI_model(
    dataset = dataset,
    genes_id = genes_id,
    corr_test = "spearman",
    significance_corr = 0.05,
    g_interactome = mock_interactome,
    tax_ID = 9606,
    na_as_zero = TRUE,
    zero_as_na = FALSE,
    compute_weights = TRUE
  )

  # Verify the output graph
  expect_true(igraph::is_igraph(result))
  expect_equal(length(igraph::V(result)), 4)

  # Test warning when both na_as_zero and zero_as_na are TRUE
  expect_error(corr_PPI_model(
    dataset = dataset,
    genes_id = genes_id,
    na_as_zero = TRUE,
    zero_as_na = TRUE
  ), "'na_as_zero' and 'zero_as_na' are both TRUE")

  # Check weights in the result graph
  expect_equal(names(edge.attributes(result)), "weights")
})

