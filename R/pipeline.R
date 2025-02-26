#' Pipeline for Proteomics Data Analysis
#'
#' This function implements a multi-step pipeline for analyzing a dataset, which includes data preprocessing, normalization, quantitative analysis, network topology analysis, and visualization using violin plots.
#'
#' @param dataset A data frame where rows represent proteins and columns represent samples.
#' @param names_of_groups A character vector specifying the groups within `colnames(dataset)` for grouping the data.
#' @param gene_column The name or index of the column containing gene identifiers. Default is 1.
#' @param norm_type The type of normalization to apply. Default is "TotSigNorm".
#' @param quantile_critical_nodes A numeric value specifying the quantile threshold for identifying critical nodes. Default is 0.75.
#' @param significance_manova A numeric argument indicating the significance threshold for Multivariate Analysis of Variance
#' @param fun_list A named list of functions for calculating centralities. Default is a set of common centrality measures.
#' @param g_interactome An `igraph` object representing the interactome of the species, used for mapping protein interactions.
#' @param categories A character vector specifying enrichment categories for functional analysis. Default is `c("Component", "Function", "Process", "RCTM", "WikiPathway")`.
#' @param tax_ID An integer representing the taxonomy ID for the organism (e.g., 9606 for human).
#' @param save_results_as Directory name where results will be saved. Defaults to "PiProteline_report".
#' @param ... Additional arguments passed to other functions within the pipeline.
#'
#' @return A list containing results from different steps of the analysis, including:
#' \describe{
#'   \item{descriptiveStatistics}{Data grouped, column and row statistics, correlation between groups, and indices like NSAF and emPAI.}
#'   \item{quantitativeAnalysis}{Results of manova analysis, DAve and DCI indices, and fold change calculations.}
#'   \item{networkAnalysis}{Unweighted and weighted PPI analysis, centralities, critical nodes, and violin plots.}
#'   \item{functionalAnalysis}{Results of the functional analysis, including enrichment and intersecting enrichment terms.}
#' }
#' @importFrom igraph degree betweenness as_adjacency_matrix V edge.attributes graph_from_edgelist
#' @export
#'
#' @examples
#' \dontrun{
#' # Example of how to call the pipeline function
#' dataset <- matrix(rnorm(100), nrow = 10)
#' colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2",
#'                        "control_3", "treatment_3", "control_4", "control_5",
#'                        "treatment_4", "treatment_5")
#' result <- pipeline(dataset, names_of_groups = c("control", "treatment"), tax_ID = 9606)
#' }
#'

pipeline <- function(dataset,names_of_groups,gene_column = 1,
                     norm_type = "TotSigNorm", quantile_critical_nodes = 0.75,
                     significance_manova = 0.05,
                     fun_list = c(Betweenness = igraph::betweenness,
                                  Centroids   = centroids,
                                  Bridging    = bridging_centrality),
                     g_interactome = NULL,
                     categories = c("Component","Function","Process","RCTM","WikiPathway"),
                     tax_ID = 9606,
                     save_results_as = paste0("PiProteline_report_",paste0(names_of_groups, collapse = "_")),
                     ...){


  if (is.null(g_interactome)){
    message("No interactome was given, the human interactome will be used")
    interactome_hs_filter <- filter_interactome(interactome_hs, scores_threshold = c(database = 300, experimental = 150))
    g_interactome <- graph_from_edgelist(as.matrix(interactome_hs_filter[,3:4]),directed = FALSE)
  }

  message("preprocessing data")
  preprocData <- preprocessing_data(dataset,names_of_groups,gene_column,norm_type,...)

  message("computing descriptive statistics")
  descriptiveStatistics <- descriptive_statistics(data_unique = preprocData$data_unique,
                                                  data_grouped_full = preprocData$data_grouped_full,
                                                  ... = ...)

  message("computing quantitative analysis")
  quantitativeAnalysis  <- quantitative_analysis(dataset = preprocData$data_norm,
                                                names_of_groups = names_of_groups,
                                                gene_column = 1,
                                                data_grouped_full = preprocData$data_grouped_full,
                                                significance_manova = significance_manova,
                                                ... = ...)

  message("computing network analysis")
  networkAnalysis <- network_analysis(data_grouped = preprocData$data_grouped,
                                      data_grouped_even_dim = preprocData$data_grouped_even_dim,
                                      fun_list = fun_list,
                                      g_interactome = g_interactome,
                                      quantile_critical_nodes = quantile_critical_nodes,
                                      ... = ...)

  message("computing functional analysis ")
  functionalAnalysis <- functional_analysis(dataset = preprocData$data_unique,
                                            manova_pairwise_results = quantitativeAnalysis$manova_pairw_results,
                                            unweighted_CN = networkAnalysis$Unweighted_criticalNodes,
                                            weighted_CN   = networkAnalysis$Weighted_criticalNodes,
                                            names_of_groups,tax_ID, categories, ... = ...)

  if (!is.null(save_results_as)){
    tryCatch({
      message("\nSaving the results")
      save_results(quantitativeAnalysis,
                   networkAnalysis,
                   functionalAnalysis,
                   save_results_as = save_results_as)
    }, error = function(e){
      message(e)
    })
  }

  return(list(descriptiveStatistics = descriptiveStatistics,
              quantitativeAnalysis = quantitativeAnalysis,
              networkAnalysis      = networkAnalysis,
              functionalAnalysis   = functionalAnalysis
              ))
}
