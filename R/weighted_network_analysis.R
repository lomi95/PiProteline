#' Weighted Network Analysis
#'
#' This function performs network analysis on weighted networks derived from grouped biological data. It calculates centralities, identifies hubs and bottlenecks, and analyzes centrality over quantiles for weighted networks.
#'
#' @aliases WNA
#'
#' @param data_grouped_even_dim A list of data frames representing grouped data, with same numbero of genes. If `NULL`, data is grouped using `data_unique` and the specified parameters.
#' @param names_of_groups A character vector specifying the names of the groups for analysis.
#' @param data_unique A data frame with unique data used for grouping, if `data.grouped` is `NULL`. Default is `NULL`.
#' @param fun_list A list of functions for calculating network centralities.
#' @param g_interactome An igraph object representing the interactome network.
#' @param quantile_critical_nodes A numeric value specifying the quantile threshold for identifying critical nodes.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A list containing:
#' \describe{
#'   \item{Centralities}{A list of centrality measures for weighted networks.}
#'   \item{CriticalNodes}{Lists of critical nodes (hubs and bottlenecks) in weighted networks.}
#' }
#' @export
#'
#' @examples
#' library(igraph)
#' filteredInteractome <- filter_interactome(interactome_hs,
#'                                           scores_threshold = c("experimental" = 0.15,
#'                                                                "database"    = 0.35))
#' g_interactome <- graph_from_edgelist(as.matrix(filteredInteractome[,3:4]), directed = FALSE)
#' # Example data
#' data.grouped <- list(Group1 = matrix(runif(2000), nrow = 200),
#'                       Group2 = matrix(runif(2000), nrow = 200))
#'
#' G <- sample(names(V(g_interactome)), 200)
#' data.grouped <- lapply(data.grouped, function(x) {
#'   rownames(x) <- G
#'   return(x)
#' })
#' colnames(data.grouped$Group1) <- paste0("Group1_",1:10)
#' colnames(data.grouped$Group2) <- paste0("Group2_",1:10)
#' result <- weighted_network_analysis(data_grouped_even_dim = data.grouped,
#'                                     names_of_groups = c("Group1", "Group2"),
#'                                     fun_list = list(Degree = igraph::degree,
#'                                                     Betweenness = igraph::betweenness),
#'                                     g_interactome = g_interactome,
#'                                     quantile_critical_nodes = 0.8)
#'
#'
weighted_network_analysis <- function(data_grouped_even_dim = NULL, names_of_groups, data_unique = NULL,
                                      fun_list, g_interactome = NULL, quantile_critical_nodes, ...) {
  args_list <- list(...)
  names(names_of_groups) <- names_of_groups

  if (is.null(g_interactome)){
    message("No interactome was given, the human interactome will be used")
    g_interactome <- graph_from_edgelist(as.matrix(interactome_hs[,3:4]),directed = FALSE)
  }

  search_mode <- c("","s","cs")
  names(search_mode) <- c("NotSpecific","Specific","CentralitySpecific")

  if (is.null(data_grouped_even_dim)) {
    if (is.null(data_unique)) {
      stop("Either 'data_unique' or 'data_grouped_even_dim' must be provided")
    }
    args_group_listing <- args_list[intersect(names(args_list), names(formals(group_listing)))]
    data_grouped_even_dim <- do.call(group_listing,
                                    c(list(dataset = data_unique,
                                           names_of_groups = names_of_groups,
                                           freq = 1,
                                           just_shared_genes = TRUE),
                                      args_group_listing))
  }

  # Generate PPI networks based on correlation models
  args_corr_PPI_model <- args_list[intersect(names(args_list), names(formals(corr_PPI_model)))]
  PPI_correlations <- lapply(data_grouped_even_dim, function(x) {

    suppressWarnings(do.call(corr_PPI_model, c(list(dataset = PiProteline::transpose(x, gene_column = 0),
                                                    g_interactome = g_interactome,
                                                    args_corr_PPI_model))))

  })

  if (any(sapply(PPI_correlations, function(x) all(is.na(edge.attributes(x)$weights))))){
    allp <- lapply(PPI_correlations, function(x) names(V(x))) %>%
      unlist() %>%
      unique() %>%
      sort
    Weighted <- data.frame()
    Weighted.q <- matrix(F,nrow = length(allp), ncol = length(fun_list)*length(names_of_groups),
                         dimnames = list(allp, paste0(names(fun_list), "_", rep(names_of_groups, each = length(fun_list)))))

  } else {
    args_centrality_quantiles <-  args_list[intersect(names(args_list),
                                                      names(formals(centrality_quantiles)))]

    Weighted <- lapply(PPI_correlations, function(x) {
      x <- suppressMessages(biggest_component(x))
      WaD <- suppressMessages(weights_as_distances(graph = x))
      centrality_quantiles(x, fun_list = fun_list, quantiles = 0, weights = WaD,
                           weights_as_distances = WaD, weights_wd = names(igraph::edge.attributes(x))[1],
                           args_centrality_quantiles)
    })

    # Centrality analysis over quantiles
    Weighted.q <- centralities_over_quantiles(centralities = Weighted, merge_all = FALSE,
                                              quantile_critical_nodes = quantile_critical_nodes)

  }
  # Calculate centralities for weighted networks

  # Identify critical nodes
  weigthed_CN <- lapply(search_mode, function(x) {

    HB <- critical_nodes(Weighted.q, names_of_groups, centralities = names(fun_list),
                        searching_mode = x)

  })

  return(list(PPI_correlations = PPI_correlations,
              Centralities = Weighted,
              CriticalNodes = weigthed_CN))
}
