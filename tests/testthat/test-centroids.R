test_that("centroids computes correctly", {
  library(igraph)

  # Create a small test graph
  g <- make_ring(10)  %>%
    add_edges(c(1, 5, 2, 6))

  # Test with unweighted distances and no parallelization
  centroids_no_parallel <- centroids(g, parallel = FALSE)

  # Check that the output is a numeric vector of the correct length
  expect_type(centroids_no_parallel, "double")
  expect_equal(length(centroids_no_parallel), vcount(biggest_component(g)))

  # Test with weighted distances
  E(g)$weight <- runif(ecount(g), 1, 10)  # Assign random weights
  centroids_weighted <- centroids(g, weights_as_distances = E(g)$weight, parallel = FALSE)

  # Check that the output is a numeric vector of the correct length
  expect_type(centroids_weighted, "double")
  expect_equal(length(centroids_weighted), vcount(biggest_component(g)))

})
