test_that("bridging_centrality computes correctly for unweighted connected graphs", {
  g <- igraph::make_ring(10) %>%
    igraph::add_edges(c(1, 5, 2, 6))

  bc <- bridging_centrality(g)


  expect_type(bc, "double")
  expect_equal(round(bc,6),c(2.714286, 2.142857, 0.600000, 0.600000, 2.142857, 2.714286, 3.600000, 1.500000, 1.500000, 3.600000))
  expect_equal(bc,bridging_centrality(g, betweenness(g)))

})

test_that("bridging_centrality computes correctly for unweighted disconnected graphs", {
  g <- igraph::make_ring(10) %>%
    igraph::add_edges(c(1, 5, 2, 6))

  bc <- bridging_centrality(g)

  expect_equal(round(bc,6),c(2.714286, 2.142857, 0.600000, 0.600000, 2.142857, 2.714286, 3.600000, 1.500000, 1.500000, 3.600000))
  expect_equal(bc,bridging_centrality(g, betweenness(g)))

})

test_that("bridging_centrality handles weighted graphs", {
  g <- igraph::make_ring(10) %>%
    igraph::add_edges(c(1, 5, 2, 6))
  weights_as_distances <- c(2.869602, 8.513340, 9.500821, 9.686164, 6.844087, 7.608864, 4.790010, 6.505646, 2.529435, 8.257889, 4.566519, 8.026352)

  B <- betweenness(g,weights = weights_as_distances)
  # Degree weighted
  bc.degW <- bridging_centrality(g, weights_as_distances = weights_as_distances)
  expect_equal(round(bc.degW,6), c(4.571767, 3.358177, 0.000000, 0.000000, 2.391170, 2.479836, 1.884456, 1.509569, 2.442866, 3.920608))

  # Non NULL betweenness
  expect_equal(bc.degW,bridging_centrality(g, betw = B,weights_as_distances = weights_as_distances))
  # Non NULL range weights
  expect_equal(bc.degW,bridging_centrality(g, betw = B,weights_as_distances = weights_as_distances,
                                           range_weights = range(weights_as_distances)))


  # Degree not weighted
  bc.degNW <- bridging_centrality(g, weights_as_distances = weights_as_distances, weighted_degree = F)
  expect_equal(round(bc.degNW,6), c(4, 2.857143, 0, 0, 2, 2, 2.4, 1.5, 2.5, 4.8))

  # Non NULL betweenness
  expect_equal(bc.degNW,bridging_centrality(g, betw = B,weights_as_distances = weights_as_distances,
                                            weighted_degree = F))
  # Non NULL range weights
  expect_equal(bc.degNW,bridging_centrality(g, betw = B,weights_as_distances = weights_as_distances,
                                            weighted_degree = F, range_weights = range(weights_as_distances)))



})


test_that("bridging_centrality handles correctly range weights", {
  g <- igraph::make_ring(10) %>%
    igraph::add_edges(c(1, 5, 2, 6))
  weights_as_distances <- c(2.869602, 8.513340, 9.500821, 9.686164, 6.844087, 7.608864, 4.790010, 6.505646, 2.529435, 8.257889, 4.566519, 8.026352)

  bc <- bridging_centrality(g, weights_as_distances = weights_as_distances,range_weights = c(0,10))
  expect_equal(round(bc,6), c(4.575800, 3.357376, 0, 0, 2.388983, 2.474099, 1.888932, 1.509250, 2.444838, 3.923611))

  expect_error(bridging_centrality(g, weights_as_distances = weights_as_distances, range_weights = c(0,2)),
               "The maximum of 'weights_as_distances' cannot be greater than the maximum of 'range_weights'.")
  expect_error(bridging_centrality(g,weights_as_distances = weights_as_distances,range_weights = 10),
               "The minimum of 'range_weights' cannot be greater than the minimum of 'weights_as_distances'.")
})

test_that("bridging_centrality stops on invalid input", {

  expect_error(bridging_centrality(NULL), "Input must be an igraph object.")
  expect_error(bridging_centrality("not_a_graph"), "Input must be an igraph object.")
})


