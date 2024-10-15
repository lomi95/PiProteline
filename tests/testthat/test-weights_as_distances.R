test_that("weights_as_distances works with graph input", {
  library(igraph)
  g <- make_ring(5)
  E(g)$weight <- c(1, 2, 3, 4, 5)

  result <- weights_as_distances(graph = g)
  expect_equal(length(result), 5)
  expect_true(all(result >= 0))
})

test_that("weights_as_distances works with originalWeights input", {
  original_weights <- c(10, 20, 30, 40, 50)
  result <- weights_as_distances(originalWeights = original_weights)

  expect_equal(length(result), 5)
  expect_true(all(result >= 0))
})

test_that("weights_as_distances handles negative weights", {
  original_weights <- c(-10, -20, 30, 40, 50)
  result <- weights_as_distances(originalWeights = original_weights)

  expect_equal(length(result), 5)
  expect_true(all(result >= 0))
})

test_that("weights_as_distances throws error for invalid range.weights", {
  original_weights <- c(10, 20, 30, 40, 50)
  expect_error(weights_as_distances(originalWeights = original_weights, range.weights = c(-1, 10)),
               "Minimum of range.weights is set as negative")
})

test_that("weights_as_distances adds small value when minimum weight is zero", {
  original_weights <- c(0, 10, 20, 30, 40)
  result <- weights_as_distances(originalWeights = original_weights)

  expect_true(all(result > 0))
})
