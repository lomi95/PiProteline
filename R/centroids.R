#' centroids
#'
#' @param graph igraph object
#' @param weights_as_distances weights (as distances) used to compute the shortest paths.
#'    If NULL centroids will be computed unweighted. Default = NULL
#' @param parallel if TRUE it will be used 'num_cores' to parallel the process.
#'     Default = T
#' @param num_cores number of cores to be used, ignored if parallel = FALSE.
#'      Default = round(3*detectCores()/4)
#'
#'
#' @importFrom parallel detectCores
#' @importFrom igraph components
#' @importFrom igraph delete_vertices
#' @importFrom igraph distances
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom parallel stopCluster
#'
#' @return centroids vector for all nodes
#'
#' @export
#'
centroids <- function(graph,
                      weights_as_distances = NULL,
                      parallel = T,
                      num_cores = round(3*detectCores()/4)){

  cg <- components(graph)
  if (cg$no>1){
    message("The graph is not strongly connected, the analysis will be computed on the biggest component")
    g <- delete_vertices(graph,cg$membership != which.max(cg$csize))
  } else {
    g <- graph
  }
  if (is.null(weights_as_distances)){
    dist.mat <- distances(g)
  } else {
    dist.mat <- distances(g,weights = weights_as_distances)
  }

  my_function <- function(col.x){
    diff.distances <- col.x - dist.mat
    gammaVw <- colSums(diff.distances < 0)
    gammaWv <- colSums(diff.distances > 0)

    return(min(gammaVw - gammaWv))
  }

  if (parallel){


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
