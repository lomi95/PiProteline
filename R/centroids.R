#' Compute Graph Centroids
#'
#' This function computes the centroids for all nodes in a graph based on the shortest path distances.
#' Centroids are nodes that minimize the difference between the number of nodes closer to them and
#' the number of nodes farther from them.
#'
#' The function can compute centroids using parallel processing to speed up computation, especially
#' for large graphs.
#'
#' @param graph An `igraph` object representing the graph.
#' @param weights_as_distances A vector of edge weights to be used as distances for computing the
#'     shortest paths. If NULL, the centroids will be computed unweighted. Default is NULL.
#' @param parallel Logical, whether to use parallel processing to speed up computation. Default is TRUE.
#' @param num_cores The number of CPU cores to use for parallel computation. Ignored if `parallel = FALSE`.
#'     Default is `round(3 * detectCores() / 4)`.
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom igraph components delete_vertices distances
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#'
#' @return A numeric vector containing the centroid values for all nodes in the graph.
#'     Each value corresponds to the node's centroid score, with lower values indicating higher
#'     centrality.
#' @export
#'
#' @examples
#' library(igraph)
#' g <- make_ring(10)
#' centroids <- centroids(g, parallel = FALSE)
#' print(centroids)
#'
centroids <- function(graph,
                      weights_as_distances = NULL,
                      parallel = TRUE,
                      num_cores = round(3 * detectCores() / 4)) {

  # Check if graph has multiple components
  cg <- components(graph)
  if (cg$no > 1) {
    message("The graph is not strongly connected, the analysis will be computed on the biggest component")
    g <- delete_vertices(graph, cg$membership != which.max(cg$csize))
  } else {
    g <- graph
  }

  # Compute distance matrix
  if (is.null(weights_as_distances)) {
    dist.mat <- distances(g)
  } else {
    dist.mat <- distances(g, weights = weights_as_distances)
  }

  # Function to compute the centroid score for each node
  my_function <- function(col.x) {
    diff.distances <- col.x - dist.mat
    gammaVw <- colSums(diff.distances < 0)
    gammaWv <- colSums(diff.distances > 0)

    return(min(gammaVw - gammaWv))
  }

  # Parallel or sequential computation
  if (parallel) {
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)

    Centr <- foreach(i = 1:ncol(dist.mat), .combine = c) %dopar% {
      my_function(dist.mat[, i])
    }

    stopCluster(cl)
  } else {
    Centr <- apply(dist.mat, 2, my_function)
  }

  return(Centr)
}
