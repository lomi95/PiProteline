#' condition_specific_hubs
#'
#' @param hubsList List of Hubs or bottlenecks data.frames or another output like
#'     the one from 'centrality_quantiles' function
#'
#' @return The list of Hubs or bottlenecks data.frames filtered by specificity,
#'     e.g. if a Hubs is present on 2 elements of the list, it is removed
#' @export
#'
condition_specific_hubs <- function(hubsList){

  count.genes <- table(unlist(lapply(hubsList,rownames)))
  if (max(count.genes)>1){
    CS_H <- lapply(hubsList, function(x){
      x[setdiff(rownames(x),names(which(count.genes>1))),]
    })
  }
  return(CS_H)
}
