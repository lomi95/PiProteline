#' @title Identify Critical Nodes Based on Centrality Quantiles
#' @description This function identifies critical nodes based on centrality measures
#'      by determining whether each node's centrality value exceeds a specified quantile threshold
#'      for each centrality measure across different groups.
#'
#' @param Centralities A list of data frames, where each data frame contains
#'  centrality measures for a group. Rows represent nodes,
#'  and columns represent different centrality metrics.
#' @param merge_all A logical indicating whether to include all nodes when merging
#'  centralities across groups. If `TRUE`, all nodes will be included;
#'  if `FALSE`, only nodes present in all groups will be considered.
#' @param quantile_critical_nodes A numeric value between 0 and 1 specifying
#'  the quantile threshold above which nodes are considered critical.
#'  For example, a `quantile_critical_nodes` of 0.9 indicates that nodes
#'  with centrality values in the top 10th percentile are considered critical.
#'
#' @return A logical matrix indicating whether each node's centrality value
#'  exceeds the specified quantile threshold for each centrality metric.
#'  Rows represent nodes, and columns represent the centrality metrics for different groups.
#' @importFrom stats quantile
#' @examples
#' # Example data
#' group1 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
#' rownames(group1) <- paste0("Gene", 1:100)
#' group2 <- data.frame(Degree = rnorm(100), Closeness = rnorm(100))
#' rownames(group2) <- paste0("Gene", 1:100)
#'
#' Centralities <- list(Group1 = group1, Group2 = group2)
#'
#' # Identify critical nodes based on the top 25% of centrality values
#' result <- centralities_over_quantiles(Centralities, merge_all = TRUE,quantile_critical_nodes = 0.75)
#' print(result)
#'
#' @export
centralities_over_quantiles <- function(Centralities, merge_all, quantile_critical_nodes){

  merge_by_rownames <- function(x,y){
    x$GeneName <- rownames(x)
    y$GeneName <- rownames(y)
    xy <- merge(x,y, by = "GeneName", all = merge_all)
    rownames(xy) <- xy$GeneName
    xy$GeneName <- NULL
    return(xy)
  }

  if (is.null(names(Centralities))){
    names_of_groups <- paste0("Group_",1:length(Centralities))
  } else {
    names_of_groups <- names(Centralities)
  }
  names(names_of_groups) <- names_of_groups

  Centralities.1 <- lapply(names_of_groups, function(x){
    colnames(Centralities[[x]]) <- paste0(colnames(Centralities[[x]]),"_",x)
    return(Centralities[[x]])
  })

  Centralities.2 <- Reduce(merge_by_rownames,Centralities.1)
  Centralities.q <- apply(Centralities.2,2, function(x) x > quantile(x,quantile_critical_nodes,na.rm = T))
  return(Centralities.q)
}
