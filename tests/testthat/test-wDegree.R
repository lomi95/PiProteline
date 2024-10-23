test_that("degree calculates unweighted degree correctly", {
  library(igraph)
  # Create a simple graph
  g <- make_ring(5)

  # Calculate unweighted degree
  result <- wDegree(g)

  # Check that the result is a numeric vector
  expect_type(result, "double")

  # Verify that the degree is correct
  expect_equal(result, degree(g))
})

test_that("degree calculates weighted degree correctly", {
  library(igraph)
  # Create a simple graph with weights
  g <- make_ring(5)
  E(g)$weight <- c(1, 2, 3, 4, 5)

  # Calculate weighted degree
  result <- wDegree(graph = g, weights_wd = "weight")

  # Check that the result is a numeric vector
  expect_type(result, "double")

  # Verify that the weighted degree is correct
  expect_equal(result, c(6, 3, 5, 7, 9))
})

test_that("degree handles graphs with no weights correctly", {
  library(igraph)
  # Create a graph with no weights
  g <- make_star(6, mode = "undirected")

  # Calculate unweighted degree
  result <- wDegree(g)

  # Check that the result is a numeric vector
  expect_type(result, "double")

  # Verify that the degree is correct (central node should have degree 5, others 1)
  expect_equal(result, c(5, 1, 1, 1, 1, 1))
})

test_that("degree calculates weighted degree when all weights are zero", {
  library(igraph)
  # Create a simple graph with zero weights
  g <- make_ring(5)
  E(g)$weight <- rep(0, ecount(g))

  # Calculate weighted degree
  result <- wDegree(g, weights_wd = "weight")

  # Check that the result is a numeric vector
  expect_type(result, "double")

  # Verify that the weighted degree is zero for all nodes
  expect_equal(result, rep(0, 5))
})

test_that("degree handles additional arguments correctly", {
  library(igraph)
  # Create a directed graph
  g <- make_ring(5, directed = TRUE)

  # Calculate in-degree
  result_in <- wDegree(g, mode = "in")

  # Calculate out-degree
  result_out <- wDegree(g, mode = "out")

  # Check that results are numeric vectors
  expect_type(result_in, "double")
  expect_type(result_out, "double")

  # Verify that the in-degree and out-degree are correct
  expect_equal(result_in, rep(1, 5))
  expect_equal(result_out, rep(1, 5))
})

test_that("degree returns correct results for weighted graphs with custom weights", {
  library(igraph)
  # Create a weighted graph
  g <- make_ring(5)
  E(g)$custom_weight <- c(1, 2, 3, 4, 5)

  # Calculate weighted degree using custom weights
  result <- wDegree(g, weights_wd = "custom_weight")

  # Check that the result is a numeric vector
  expect_type(result, "double")

  # Verify that the weighted degree is calculated correctly
  expect_equal(result, c(6, 3, 5, 7, 9))
})

test_that("degree handles graphs with isolated nodes", {
  library(igraph)
  # Create a graph with an isolated node
  g <- make_ring(4)
  g <- add_vertices(g, 1) # Add an isolated node

  # Calculate unweighted degree
  result <- degree(g)

  # Check that the result includes the isolated node
  expect_equal(length(result), 5)

  # Verify that the degree of the isolated node is zero
  expect_equal(result[5], 0)
})
