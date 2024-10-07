#' Biggest Component of a Graph
#'
#' This function checks the number of components in a graph and returns the largest
#' component if there is more than one.
#'
#' @param graph An `igraph` object representing the graph to be analyzed.
#'
#' @importFrom igraph components
#' @importFrom igraph delete_vertices
#'
#' @return An `igraph` object representing the biggest component of the input graph.
#' @export
#'
#' @examples
#' library(igraph)
#' g <- make_ring(10) %>%
#'   add_vertices(5) %>%
#'   add_edges(c(1, 11, 1, 12, 1, 13, 1, 14))
#' biggest_graph <- biggest_component(g)
#'
biggest_component <- function(graph){
  inherit_igraph(graph)

  cg <- components(graph)
  if (cg$no > 1) {
    message("The graph is not strongly connected. The analysis will be computed on the biggest component.")
    graph <- delete_vertices(graph, cg$membership != which.max(cg$csize))
  }

  return(graph)
}
