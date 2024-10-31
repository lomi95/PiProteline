#' Network Analysis for Biological Data
#'
#' This function performs network analysis on grouped biological data, including the identification of critical nodes in unweighted and weighted networks. It calculates centralities, identifies hubs and bottlenecks, and optionally generates violin plots for random networks.
#'
#' @param data.grouped A list of data frames representing grouped data. If `NULL`, data is grouped using `data.unique` and the specified parameters.
#' @param data.grouped.evenDim A list of data frames representing grouped data with even dimensions. Used for weighted network analysis. Default is `NULL`.
#' @param fun_list A list of functions for calculating network centralities.
#' @param g.interactome A igraph object representing the interactome.
#' @param quantile_critical_nodes A numeric value specifying the quantile threshold for identifying critical nodes.
#' @param names_of_groups A character vector specifying the names of the groups. If `NULL`, it is derived from `data.grouped` or must be provided with `data.unique`. Default is `NULL`.
#' @param data.unique A data frame with unique data used for grouping, if `data.grouped` is `NULL`. Default is `NULL`.
#' @param violins A logical value indicating whether to generate violin plots for random networks. Default is `FALSE`.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A list containing:
#' \describe{
#'   \item{PPI_unweighted}{A list of unweighted PPI (protein-protein interaction) subgraphs for each group.}
#'   \item{PPI_correlations}{A list of weighted PPI subgraphs based on correlation models.}
#'   \item{Unweighted_centralities}{A list of centrality measures for unweighted networks.}
#'   \item{Unweighted_criticalNodes}{Lists of critical nodes (hubs and bottlenecks) in unweighted networks.}
#'   \item{Weighted_centralities}{A list of centrality measures for weighted networks.}
#'   \item{Weighted_criticalNodes}{Lists of critical nodes in weighted networks.}
#'   \item{RandomDist_Violins}{Violin plots for random network distributions (if `violins = TRUE`).}
#' }
#' @export
#'
#' @examples
#' # Example data
#' library(igraph)
#' \dontrun{
#'
#' data.unique <- matrix(runif(4000), nrow = 200)
#' rownames(data.unique) <- sample(names(V(g.interactome)), nrow(data.unique))
#' colnames(data.unique) <- c(paste0("Group1_",1:10),paste0("Group2_",1:10))
#'
#' # Perform network analysis
#' network_analysis(data.unique = data.unique,
#'                  fun_list = list(bridging = bridging_centrality,
#'                                  centroids = centroids,
#'                                  betweenness = betweenness),
#'                  quantile_critical_nodes = 0.9, violins = FALSE)
#' }
#'
network_analysis <- function(data.grouped = NULL, data.grouped.evenDim = NULL, fun_list, g.interactome,
                             quantile_critical_nodes, names_of_groups = NULL, data.unique = NULL,violins = F,...){
  args_list <- list(...)

  if (is.null(names_of_groups)){
    if (is.null(data.unique)){
      names_of_groups <- names(data.grouped)
      names(names_of_groups) <- names_of_groups
    } else {
      stop("'names_of_groups' should be provided with 'data.unique'")
    }
  }

  unweightedNetwAnalysis <- unweighted_network_analysis(data.grouped = data.grouped, data.unique = data.unique,
                                                        names_of_groups = names_of_groups,fun_list = fun_list,
                                                        g.interactome = g.interactome,
                                                        quantile_critical_nodes = quantile_critical_nodes, args_list)

  weightedNetwAnalysis <- weighted_network_analysis(data.grouped.evenDim = data.grouped.evenDim, data.unique = data.unique,
                                                    names_of_groups = names_of_groups,fun_list = fun_list,
                                                    g.interactome = g.interactome,
                                                    quantile_critical_nodes = quantile_critical_nodes, args_list)

  if (violins){
    vd <- vb <- vwb <- NULL
    try(vd  <- suppressMessages(lapply(unweightedNetwAnalysis$PPI_unweighted,Violin_degreeRandom)))
    try(vb  <- suppressMessages(lapply(unweightedNetwAnalysis$PPI_unweighted,Violin_betweennessRandom)))
    try(vwb <- suppressMessages(lapply(weightedNetwAnalysis$PPI_correlations, function(x){
      Violin_betweennessRandom(x,Weights = weights_as_distances(graph = x))
    })))


    Violins <- list(RandomDegree                 = vd,
                    RandomBetweenness_unweighted = vb,
                    RandomBetweenness_weighted   = vwb)

    return(list(PPI_unweighted           = unweightedNetwAnalysis$PPI_unweighted,
                PPI_correlations         = weightedNetwAnalysis$PPI_correlations,
                Unweighted_centralities  = unweightedNetwAnalysis$Centralities,
                Unweighted_criticalNodes = unweightedNetwAnalysis$CriticalNodes,
                Weighted_centralities    = weightedNetwAnalysis$Centralities,
                Weighted_criticalNodes   = weightedNetwAnalysis$CriticalNodes,
                RandomDist_Violins       = Violins))
  }

  return(list(PPI_unweighted           = unweightedNetwAnalysis$PPI_unweighted,
              PPI_correlations         = weightedNetwAnalysis$PPI_correlations,
              Unweighted_centralities  = unweightedNetwAnalysis$Centralities,
              Unweighted_criticalNodes = unweightedNetwAnalysis$CriticalNodes,
              Weighted_centralities    = weightedNetwAnalysis$Centralities,
              Weighted_criticalNodes   = weightedNetwAnalysis$CriticalNodes))
}
