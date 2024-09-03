#' NSAF
#'
#' @param list.groups list of groups of which we want to compute NSAF index, or a dataset Samples x Proteins
#' @param length_proteins length of proteins, ordered according to the datasets
#'
#' @return A matrix/vector of nsaf indexes for each protein
#' @export
#'
NSAF <- function(list.groups, length_proteins){
  if (is.list(list.groups)){
    nsaf_index <- sapply(list.groups, function(x){
      (colMeans(x)/length_proteins)/sum(colMeans(x)/length_proteins)
    })
  } else {
    nsaf_index <- (colMeans(list.groups)/length_proteins)/sum(colMeans(list.groups)/length_proteins)
  }

  return(nsaf_index)
}
