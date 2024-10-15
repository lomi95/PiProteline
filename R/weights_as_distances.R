#' Convert Edge Weights to Distances
#'
#' The function `weights_as_distances` takes a set of edge weights and converts them into distances.
#' This is done by reversing the weight values, so that higher original weights become smaller distances.
#' If the minimum weight is zero, a small constant is added to all weights to avoid issues with betweenness computations.
#'
#' @param originalWeights A numeric vector with the original weights of the edges. If `NULL`, the first edge attribute from the `graph` is used. Default is `NULL`.
#' @param graph An `igraph` object. Not necessary if `originalWeights` is provided. If both are `NULL`, an error will occur. Default is `NULL`.
#' @param range.weights A numeric vector of length two specifying the range for the weights. If `NULL`, the range is set to `range(originalWeights)`. Default is `NULL`.
#'
#' @importFrom igraph edge.attributes
#'
#' @return A numeric vector containing the transformed weights (distances).
#' @examples
#' # Example with a graph
#' library(igraph)
#' g <- make_ring(5)
#' E(g)$weight <- c(1, 2, 3, 4, 5)
#' weights_as_distances(graph = g)
#'
#' # Example with original weights
#' original_weights <- c(10, 20, 30, 40, 50)
#' weights_as_distances(originalWeights = original_weights)
#'
#' @seealso [igraph::edge.attributes]
#' @export
weights_as_distances <- function(originalWeights = NULL, graph = NULL, range.weights = NULL) {
  if (!is.null(graph)) {
    if (is.null(originalWeights)) {
      originalWeights <- edge.attributes(graph)[[1]]
    }
    if (is.null(originalWeights)) {
      stop("No weights are available")
    }
  }

  if (min(originalWeights) < 0) {
    originalWeights <- abs(originalWeights)
    message("Some of the original weights were negative, all weights will be taken as their absolute values")
  }

  if (is.null(range.weights)) {
    Weights <- max(originalWeights) - originalWeights
  } else {
    if (min(range.weights) <= 0) {
      stop("Minimum of range.weights is set as negative")
    }
    Weights <- max(range.weights) - originalWeights
  }

  if (suppressWarnings(min(Weights) == 0)) {
    Weights <- Weights + min(unique(sort(Weights))[2] / 10, 0.001)
  }

  return(Weights)
}
