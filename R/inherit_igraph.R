#' check if is igraph
#'
#' @param graph
#'
#' @return
#'
#' @examples
inherit_igraph <- function(graph){
  if (!inherits(graph, "igraph")) {
    stop("Input must be an igraph object.")
  }
}
