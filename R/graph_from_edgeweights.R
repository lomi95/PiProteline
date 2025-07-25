#' Construct a Graph from an Edge List with Weights
#'
#' This function creates an igraph object from a provided edge list that includes a "weights" column.
#' The first two columns of the data frame are interpreted as the source and target vertices,
#' while the "weights" column specifies the weight for each edge.
#'
#' @param el A data frame representing the edge list. The first two columns must contain the node identifiers,
#'   and there must be a column named "weights" containing numeric edge weights.
#' @param directed Logical indicating whether the resulting graph should be directed. Defaults to \code{FALSE}.
#'
#' @return An igraph object with the specified edges and their associated weights.
#'
#' @details The function uses \code{graph_from_edgelist} from the igraph package to convert
#' the first two columns of \code{el} into an igraph object. It then assigns the edge weights from
#' the \code{"weights"} column of \code{el} to the corresponding edges in the graph.
#'
#' @export
#'
#' @examples
#' # Sample edge list data frame
#' el <- data.frame(
#'   from = c("A", "B", "C"),
#'   to   = c("B", "C", "A"),
#'   weights = c(1.2, 3.4, 2.1)
#' )
#'
#' # Create an undirected graph with weighted edges
#' g <- graph_from_edgeweights(el, directed = FALSE)
#'
#' # Print graph summary
#' print(g)

graph_from_edgeweights <- function(el, directed = F){

  g <- igraph::graph_from_edgelist(as.matrix(el[,1:2]), directed = directed)

  igraph::edge.attributes(g)$weights <- el[,"weights"]

  return(g)
}
