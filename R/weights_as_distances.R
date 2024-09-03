#' weights_as_distances
#'
#' @param graph igraph object. Not necessary if 'originalWeights' is given. Default = NULL
#' @param originalWeights A vector with the node weights. If NULL it will be
#'     used the first edge.attributes of 'graph'. Default = NULL
#' @param range.weights The range the should have the weights. If NULL it will
#'     be used range(originalWeights) . Default = NULL
#'
#'
#' @importFrom igraph edge.attributes
#'
#' @return A vector containing the weights
#' @export
#'
weights_as_distances <- function(originalWeights = NULL, graph = NULL, range.weights = NULL){

  if (!is.null(graph)){
    if (is.null(originalWeights)){
      originalWeights <- edge.attributes(graph)[[1]]
    }
    if (is.null(originalWeights)){
      stop("No weights are avaliable")
    }
  }

  if (min(originalWeights)<0){
    originalWeights <- abs(originalWeights)
    message("Some of originals weights were negative, all weights will be taken as their absolute values")
  }




  if (is.null(range.weights)){
    Weights <- max(originalWeights) - originalWeights

  } else {
    if (min(range.weights) <= 0){
      stop("minimum of range.weights is set as negative")
    }
    Weights <- max(range.weights) - originalWeights
  }


  # If the minimum weight is 0 the betwenness can't be computed, so we add the
  # same small quantity to all weights
  if (suppressWarnings(min(Weights) == 0)){
    Weights <- Weights + min(unique(sort(Weights))[2]/10,0.001)
  }

  return(Weights)
}
