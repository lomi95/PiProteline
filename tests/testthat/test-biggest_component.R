test_that("biggest_component works correctly with connected graph", {
  # Create connected graph
  g_connected <- igraph::make_ring(10)
  bc <- biggest_component(g_connected)

  # Check number of nodes and edges
  expect_s3_class(bc, "igraph")
  expect_true(igraph::components(bc)$no == 1)
  expect_equal(igraph::vcount(bc), igraph::vcount(g_connected))
  expect_equal(igraph::ecount(bc), igraph::ecount(g_connected))

  # Expect no message
  expect_no_message(biggest_component(g_connected))
})

test_that("biggest_component extracts the largest component from disconnected graph", {
  # Create disconnected graph
  g_disconnected <- igraph::make_ring(10) + igraph::make_ring(5)
  bc <- biggest_component(g_disconnected)

  # Biggest component has 10 nodes
  expect_s3_class(bc, "igraph")
  expect_true(igraph::components(bc)$no == 1)
  expect_equal(igraph::vcount(bc), 10)
  expect_equal(igraph::ecount(bc), 10)

  # Expect message
  expect_message(biggest_component(g_disconnected),"The graph is not strongly connected. The analysis will be computed on the biggest component.")
})

test_that("biggest_component throws error for non-igraph input", {
  # Non valid input
  expect_error(biggest_component(NULL), "Input must be an igraph object.")
  expect_error(biggest_component("not a graph"), "Input must be an igraph object.")
})
