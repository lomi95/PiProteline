test_that("Violin_degreeRandom generates a ggplot object", {
  library(igraph)
  g <- make_ring(10)

  # Generate the plot
  plot <- Violin_degreeRandom(g, nIter = 100, set_seed = 42)

  # Verify the plot is a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("Violin_degreeRandom works with different numbers of iterations", {
  library(igraph)
  g <- make_ring(10)

  # Test with different number of iterations
  plot_10 <- Violin_degreeRandom(g, nIter = 10, set_seed = 42)
  plot_200 <- Violin_degreeRandom(g, nIter = 200, set_seed = 42)

  # Verify both plots are ggplot objects
  expect_s3_class(plot_10, "ggplot")
  expect_s3_class(plot_200, "ggplot")
})

test_that("Violin_degreeRandom handles different seed values", {
  library(igraph)
  g <- make_ring(10)

  # Generate plots with different seeds
  plot1 <- Violin_degreeRandom(g, nIter = 100, set_seed = 1)
  plot2 <- Violin_degreeRandom(g, nIter = 100, set_seed = 2)

  # Check that the plots are ggplot objects
  expect_s3_class(plot1, "ggplot")
  expect_s3_class(plot2, "ggplot")

  # Check that the plots are not identical due to different seeds
  expect_false(identical(plot1, plot2))
})

test_that("Violin_degreeRandom works with small and large graphs", {
  library(igraph)
  small_graph <- make_ring(5)
  large_graph <- make_ring(100)

  # Test with a small graph
  small_plot <- Violin_degreeRandom(small_graph, nIter = 50, set_seed = 1)
  expect_s3_class(small_plot, "ggplot")

  # Test with a large graph
  large_plot <- Violin_degreeRandom(large_graph, nIter = 50, set_seed = 1)
  expect_s3_class(large_plot, "ggplot")
})

