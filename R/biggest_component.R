#' biggest_component
#'
#' @param graph check the number graph components and take the biggest one if there
#'     are more than one
#'
#' @importFrom igraph components
#' @importFrom igraph delete_vertices
#'
#' @return igraph object, the biggest component
#' @export
#'
biggest_component <- function(graph){
  cg <- components(graph)
  if (cg$no>1){
    message("The graph is not strongly connected, the analysis will be computed on the biggest component")
    graph <- delete_vertices(graph,cg$membership != which.max(cg$csize))
  }
  return(graph)

}
