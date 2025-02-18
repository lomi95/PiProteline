#' Unweighted Network Analysis
#'
#' This function performs network analysis on unweighted networks derived from grouped biological data. It calculates centralities, identifies hubs and bottlenecks, and analyzes centrality over quantiles.
#'
#' @aliases UNA
#'
#' @param data_grouped A list of data frames representing grouped data. If `NULL`, data is grouped using `data_unique` and the specified parameters.
#' @param names_of_groups A character vector specifying the names of the groups for analysis.
#' @param data_unique A data frame with unique data used for grouping, if `data_grouped` is `NULL`. Default is `NULL`.
#' @param ... Additional arguments passed to internal functions.
#' @param fun_list A list of functions for calculating network centralities.
#' @param quantile_critical_nodes A numeric value specifying the quantile threshold for identifying critical nodes.
#' @param g_interactome A igraph object representing the interactome.
#'
#' @return A list containing:
#' \describe{
#'   \item{Centralities}{Centrality measures for unweighted networks.}
#'   \item{CriticalNodes}{Lists of critical nodes (hubs and bottlenecks) in unweighted networks.}
#' }
#' @export
#'
#' @examples
#' library(igraph)
#' # Example data
#' filteredInteractome <- filter_interactome(interactome_hs,
#'                                           scores_threshold = c("experimental" = 0.15,
#'                                                                "database"    = 0.35))
#' g_interactome <- graph_from_edgelist(as.matrix(filteredInteractome[,3:4]),
#'                                      directed = FALSE)
#' set.seed(1)
#' data_grouped <- list(Group1 = matrix(runif(2000), nrow = 200),
#'                      Group2 = matrix(runif(2000), nrow = 200))
#' data_grouped <- lapply(data_grouped, function(x) {
#'   rownames(x) <- sample(names(V(g_interactome)), nrow(x))
#'   return(x)
#' })
#' colnames(data_grouped$Group1) <- paste0("Group1_",1:10)
#' colnames(data_grouped$Group2) <- paste0("Group2_",1:10)
#' result <- unweighted_network_analysis(data_grouped = data_grouped,
#'                                       names_of_groups = c("Group1", "Group2"),
#'                                       fun_list = list(Degree = igraph::degree,
#'                                                      Betweenness = igraph::betweenness),
#'                                      g_interactome = g_interactome,
#'                                      quantile_critical_nodes = 0.8)
#'
#'
unweighted_network_analysis <- function(data_grouped = NULL, names_of_groups, data_unique = NULL,
                                        fun_list, g_interactome = NULL, quantile_critical_nodes, ...){
  args_list <- list(...)

  if (is.null(g_interactome)){
    message("No interactome was given, the human interactome will be used")
    g_interactome <- graph_from_edgelist(as.matrix(interactome_hs[,3:4]),directed = FALSE)
  }

  search_mode <- c("","s","cs")
  names(search_mode) <- c("NotSpecific","Specific","CentralitySpecific")

  if (is.null(data_grouped)){
    if (is.null(data_unique)){
      stop("Either 'data_unique or 'data_grouped' must be given")
    }
    args_group_listing <- args_list[intersect(names(args_list),
                                              names(formals(group_listing)))]
    data_grouped <- do.call(group_listing,
                            c(list(dataset = data_unique,
                                   names_of_groups = names_of_groups,
                                   freq = 1),
                              args_group_listing))
  }

  PPI_unweighted <- lapply(data_grouped, function(x){
    G <- rownames(x)
    if (length(intersect(names(igraph::V(g_interactome)),G)) == 0){
      stop("No gene names were found in the interactome given")
    }
    igraph::induced_subgraph(g_interactome, intersect(names(igraph::V(g_interactome)),G))
  })

  args_centrality_quantiles <-  args_list[intersect(names(args_list),
                                                    names(formals(centrality_quantiles)))]
  Unweighted <- lapply(PPI_unweighted,function(x){
    x <- suppressMessages(biggest_component(x))
    centrality_quantiles(x, fun_list = fun_list,
                         quantiles = 0,
                         ... = ...)
  })

  Unweighted.q <- centralities_over_quantiles(centralities = Unweighted,
                                              merge_all = T,
                                              quantile_critical_nodes = quantile_critical_nodes)

  unweighted_CN <- lapply(search_mode, function(x){

    HB <- critical_nodes(Unweighted.q, names_of_groups,
                        centralities = names(fun_list),
                        searching_mode = x)
    return(HB)
  })


  return(list(PPI_unweighted = PPI_unweighted,
              Centralities = Unweighted,
              CriticalNodes = unweighted_CN))
}
