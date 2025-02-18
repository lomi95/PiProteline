#' Network Analysis for Biological Data
#'
#' This function performs network analysis on grouped biological data, including the identification of critical nodes in unweighted and weighted networks. It calculates centralities, identifies hubs and bottlenecks, and optionally generates violin plots for random networks.
#'
#' @param data_grouped A list of data frames representing grouped data. If `NULL`, data is grouped using `data_unique` and the specified parameters.
#' @param data_grouped_even_dim A list of data frames representing grouped data with even dimensions. Used for weighted network analysis. Default is `NULL`.
#' @param fun_list A list of functions for calculating network centralities.
#' @param g_interactome A igraph object representing the interactome.
#' @param quantile_critical_nodes A numeric value specifying the quantile threshold for identifying critical nodes.
#' @param names_of_groups A character vector specifying the names of the groups. If `NULL`, it is derived from `data_grouped` or must be provided with `data_unique`. Default is `NULL`.
#' @param data_unique A data frame with unique data used for grouping, if `data_grouped` is `NULL`. Default is `NULL`.
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
#' }
#' @export
#'
#' @examples
#' # Example data
#' library(igraph)
#' \dontrun{
#'
#' data_unique <- matrix(runif(4000), nrow = 200)
#' rownames(data_unique) <- sample(names(V(g_interactome)), nrow(data_unique))
#' colnames(data_unique) <- c(paste0("Group1_",1:10),paste0("Group2_",1:10))
#'
#' # Perform network analysis
#' network_analysis(data_unique = data_unique,
#'                  fun_list = list(bridging = bridging_centrality,
#'                                  centroids = centroids,
#'                                  betweenness = betweenness),
#'                  quantile_critical_nodes = 0.9, violins = FALSE)
#' }
#'
network_analysis <- function(data_grouped = NULL, data_grouped_even_dim = NULL, fun_list, g_interactome = NULL,
                             quantile_critical_nodes, names_of_groups = NULL, data_unique = NULL,...){

  if (is.null(g_interactome)){
    message("No interactome was given, the human interactome will be used")
    g_interactome <- graph_from_edgelist(as.matrix(interactome_hs[,3:4]),directed = FALSE)
  }


  if (is.null(names_of_groups)){
    if (is.null(data_unique)){
      names_of_groups <- names(data_grouped)
      names(names_of_groups) <- names_of_groups
    } else {
      stop("'names_of_groups' should be provided with 'data_unique'")
    }
  }

  unweightedNetwAnalysis <- unweighted_network_analysis(data_grouped = data_grouped, data_unique = data_unique,
                                                        names_of_groups = names_of_groups,fun_list = fun_list,
                                                        g_interactome = g_interactome,
                                                        quantile_critical_nodes = quantile_critical_nodes, ... = ...)

  weightedNetwAnalysis <- weighted_network_analysis(data_grouped_even_dim = data_grouped_even_dim, data_unique = data_unique,
                                                    names_of_groups = names_of_groups,fun_list = fun_list,
                                                    g_interactome = g_interactome,
                                                    quantile_critical_nodes = quantile_critical_nodes, ...)

  return(list(Unweighted_centralities  = unweightedNetwAnalysis$Centralities,
              Weighted_centralities    = weightedNetwAnalysis$Centralities,
              Unweighted_criticalNodes = unweightedNetwAnalysis$CriticalNodes,
              Weighted_criticalNodes   = weightedNetwAnalysis$CriticalNodes,
              PPI_unweighted           = unweightedNetwAnalysis$PPI_unweighted,
              PPI_correlations         = weightedNetwAnalysis$PPI_correlations
  ))
}
