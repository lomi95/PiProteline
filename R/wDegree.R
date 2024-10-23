#' Calculate Degree or Weighted Degree of a Graph
#'
#' This function calculates the degree of nodes in a graph, either using standard unweighted degree calculation
#'  or weighted degree based on a specified edge attribute, after taken their absolute value.
#'
#' @param graph An `igraph` object representing the graph.
#' @param weights_wd A character string specifying the name of the edge attribute to use for weighted degree calculation.
#'  If `NULL`, unweighted degree is calculated. Default is `NULL`.
#' @param ... Additional arguments to be passed to `igraph::degree` if using unweighted calculation.
#'
#' @return A numeric vector containing the degree or weighted degree for each node in the graph.
#' @export
#'
#' @examples
#' library(igraph)
#' # Create a simple graph
#' g <- make_ring(10)
#'
#' # Calculate unweighted degree
#' unweighted_deg <- wDegree(g)
#'
#' # Add weights to the edges
#' E(g)$weight <- runif(ecount(g))
#'
#' # Calculate weighted degree
#' weighted_deg <- wDegree(g, weights_wd = "weight")
#'
#' # Print the results
#' print(unweighted_deg)
#' print(weighted_deg)
wDegree <- function(graph, weights_wd = NULL, ...) {
  if (is.null(weights_wd)) {
    args_list <- list(...)
    args_degree.igraph <- args_list[intersect(names(args_list), names(formals(igraph::degree)))]
    WD <- igraph::degree(graph, ...)
  } else {
    adjM <- igraph::as_adjacency_matrix(graph, attr = weights_wd)
    WD <- rowSums(as.matrix(abs(adjM)))
  }

  return(WD)
}
