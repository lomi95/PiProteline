#' Bridging Centrality Calculation
#'
#' This function calculates the bridging centrality of each vertex in a graph. The
#' bridging centrality combines both the betweenness centrality and the "bridging
#' coefficient," a measure of how well-connected a node is relative to its neighbors.
#' The bridging coefficient is calculated following the method described by Hwang et al.
#' (2006). For weighted graphs, the formula is slightly changed: instead of using
#' the degree,  For more details, see the reference below.
#'
#' When `degree_weighted = TRUE`, instead of using the degree (i.e., the number of edges)
#' as described by Hwang et al, the weighted degree is used, which is calculated
#' as the sum of the incident edge weights on each vertex.
#'
#' @param graph An `igraph` object representing the graph.
#' @param Betw A precomputed betweenness centrality vector to speed up computation.
#'     If NULL, betweenness will be computed within the function. Default is NULL.
#' @param weights_as_distances A vector of weights to be treated as distances on the graph's edges.
#'     If NULL, unweighted bridging centrality will be computed. Default is NULL.
#' @param range_weights A numeric range indicating the desired range of weights for the computation.
#'     If NULL, the range of `weights_as_distances` will be used. Ignored if `degree_weighted = FALSE`.
#'     Default is NULL.
#' @param degree_weighted A logical indicating whether the degree should be computed as the sum of weights
#'     (when `TRUE`) or the number of edges (when `FALSE`). Default is TRUE.
#'
#' @importFrom igraph components delete_vertices edge_attr as_adjacency_matrix neighborhood betweenness set_edge_attr
#' @return A numeric vector of bridging centrality values for each vertex.
#' @export
#'
#' @references
#' Hwang, W., Cho, Y. R., Zhang, A., & Ramanathan, M. (2006). Bridging Centrality: Identifying Bridging Nodes in Scale-Free Networks. *Technical Report*. Department of Computer Science and Engineering, University at Buffalo, State University of New York. Available at: \url{https://cse.buffalo.edu/tech-reports/2006-05.pdf}.
#'
#' @examples
#' library(igraph)
#' g <- make_ring(10) %>%
#'   add_edges(c(1, 5, 2, 6))
#' bc <- bridging_centrality(g)
#'
bridging_centrality <- function(graph,
                                Betw = NULL,
                                weights_as_distances = NULL,
                                range_weights = NULL,
                                degree_weighted = T){

  inherit_igraph(graph)

  if (!is.null(range_weights)){
    if(min(range_weights) > min(weights_as_distances)) {
      stop("The minimum of 'range_weights' cannot be greater than the minimum of 'weights_as_distances'.")
    }
    if(max(range_weights) < max(weights_as_distances)) {
      stop("The maximum of 'weights_as_distances' cannot be greater than the maximum of 'range_weights'.")
    }
  }

  cg <- components(graph)
  if (cg$no>1){
    message("The graph is not strongly connected, the analysis will be computed
            on the biggest component")
    graph <- delete_vertices(graph,cg$membership != which.max(cg$csize))
  } else {
    graph <- graph
  }
  ### se sono presenti i pesi la matrice di adiacenza viene costruita con i pesi
  if (!is.null(weights_as_distances) & degree_weighted){
    graph <- set_edge_attr(graph,name = "ed.att",value = weights_as_distances)
    if (is.null(range_weights)){
      range_weights <- range(weights_as_distances)
    }
    graph.ad <- max(range_weights) - as_adjacency_matrix(graph,attr = "ed.att")

  } else {
    graph.ad <- as_adjacency_matrix(graph)
  }

  # vector that contains vertex degrees

  dgraph <- colSums(as.matrix(graph.ad))

  # First neighbors for each vertex
  firstNeighbors.1 <- neighborhood(graph,1)
  names(firstNeighbors.1) <- names(dgraph)
  firstNeighbors <- lapply(firstNeighbors.1, function(x) x[-1])

  # bridging coefficient function
  BC <- function(Dv, fN){
    return((1/Dv)/sum(1/dgraph[fN]))
  }
  BridCoeff <- mapply(BC, dgraph, firstNeighbors)


  if (is.null(Betw)){
    if (!is.null(weights_as_distances)){
      Betw <- betweenness(graph,weights = weights_as_distances)
    } else {
      Betw <- betweenness(graph)
    }
  }

  return(BridCoeff*Betw)
}
