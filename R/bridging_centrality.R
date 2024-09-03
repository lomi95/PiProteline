#' bridging_centrality
#'
#' @param graph igraph object
#' @param Betw if already computed it can be given to speed up the computation.
#'     Default = NULL
#' @param weights_as_distances weights (as distances) of all vertices.
#'     If NULL it will be computed the unweighted bridging. Default = NULL
#' @param range_weights The range that should have the weights, used to compute
#'     the weighted degree. If NULL the range is assumed to be range(weights_as_distances).
#'     Ignored if weighted degree is FALSE. Default = NULL
#' @param degree_weighted If TRUE the degree will be computed as the sum of the weights
#'    (not as distances), instead of the number of the edges
#'
#'
#' @importFrom igraph components
#' @importFrom igraph delete_vertices
#' @importFrom igraph edge_attr
#' @importFrom igraph as_adj
#' @importFrom igraph neighborhood
#' @importFrom igraph betweenness
#' @importFrom igraph set_edge_attr
#'
#' @return A bridging centrality vector
#' @export
#'
bridging_centrality <- function(graph,
                                Betw = NULL,
                                weights_as_distances = NULL,
                                range_weights = NULL,
                                degree_weighted = T){

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
    ed.att <- weights_as_distances
    graph <- set_edge_attr(graph,name = "ed.att",value = ed.att)
    if (is.null(range_weights)){
      range_weights <- range(ed.att)
    }
    graph.ad <- max(range_weights) - as_adj(graph,attr = "ed.att")

  } else {
    graph.ad <- as_adj(graph)
  }

  # vector that contains vertex degrees

  dgraph <- colSums(as.matrix(graph.ad))

  # list that contains FN vertices
  firstNeighbors.1 <- neighborhood(graph,1)
  names(firstNeighbors.1) <- names(dgraph)
  # we delete the first element, because is the vertex into consideration
  firstNeighbors <- lapply(firstNeighbors.1, function(x) x[-1])

  # bridging coefficient function -  https://cse.buffalo.edu/tech-reports/2006-05.pdf
  BC <- function(Dv, fN){
    return((1/Dv)/sum(1/dgraph[fN]))
  }
  BridCoeff <- mapply(BC, dgraph, firstNeighbors)


  if (is.null(Betw)){
    if (!is.null(weights_as_distances)){
      Betw <- betweenness(graph,weights = ed.att)
    } else {
      Betw <- betweenness(graph)
    }
  }

  return(BridCoeff*Betw)
}
