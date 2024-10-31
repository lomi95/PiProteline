#' Pipeline for Proteomics Data Analysis
#'
#' This function implements a multi-step pipeline for analyzing a dataset, which includes data preprocessing, normalization, quantitative analysis, network topology analysis, and visualization using violin plots.
#'
#' @param dataset A data frame where rows represent proteins and columns represent samples.
#' @param names_of_groups A character vector specifying the groups within `colnames(dataset)` for grouping the data.
#' @param gene_column The name or index of the column containing gene identifiers. Default is 1.
#' @param normType The type of normalization to apply. Default is "TotSigNorm".
#' @param quantile_critical_nodes A numeric value specifying the quantile threshold for identifying critical nodes. Default is 0.75.
#' @param fun_list A named list of functions for calculating centralities. Default is a set of common centrality measures.
#' @param g.interactome An `igraph` object representing the interactome of the species, used for mapping protein interactions.
#' @param violins Logical value indicating whether to create violin plots. Default is `TRUE`.
#' @param categories A character vector specifying enrichment categories for functional analysis. Default is `c("Component", "Function", "Process", "RCTM", "WikiPathway")`.
#' @param tax_ID An integer representing the taxonomy ID for the organism (e.g., 9606 for human).
#' @param ... Additional arguments passed to other functions within the pipeline.
#'
#' @return A list containing results from different steps of the analysis, including:
#' \describe{
#'   \item{descriptiveStatistics}{Data grouped, column and row statistics, correlation between groups, and indices like NSAF and emPAI.}
#'   \item{quantitativeAnalysis}{Results of LDA analysis, DAve and DCI indices, and fold change calculations.}
#'   \item{networkAnalysis}{Unweighted and weighted PPI analysis, centralities, critical nodes, and violin plots.}
#'   \item{functionalAnalysis}{Results of the functional analysis, including enrichment and intersecting enrichment terms.}
#' }
#' @importFrom igraph degree betweenness as_adjacency_matrix V edge.attributes graph_from_edgelist closeness
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
                     normType = "TotSigNorm", quantile_critical_nodes = 0.75,
                     fun_list = c(Degree      = wDegree,
                                  Betweenness = igraph::betweenness,
                                  Centroids   = centroids,
                                  Bridging    = bridging_centrality,
                                  Closeness   = igraph::closeness),
                     g.interactome = NULL,
                     violins = F,
                     categories = c("Component","Function","Process","RCTM","WikiPathway"),
                     tax_ID = 9606,
                     ...){
  args_list <- list(...)

  if (is.null(g.interactome)){
    g.interactome <- graph_from_edgelist(as.matrix(interactome_hs[,3:4]),directed = FALSE)
  }

  message("preprocessing data")
  preprocData <- preprocessing_data(dataset,names_of_groups,gene_column,normType,args_list)

  message("computing descriptive statistics")
  descriptiveStatistics <- descriptive_statistics(data.unique = preprocData$data.unique,
                                                  data.grouped = preprocData$data.grouped,
                                                  data.grouped.full = preprocData$data.grouped.full,
                                                  ... = args_list)

  message("computing quantitative analysis - ", Sys.time())
  quantitativeAnalysis  <- quantitative_analysis(dataset = preprocData$data.norm,
                                                names_of_groups = names_of_groups,
                                                gene_column = 1,
                                                data.grouped.full = preprocData$data.grouped.full,
                                                ... = args_list)

  message("computing network analysis - ", Sys.time())
  networkAnalysis <- network_analysis(data.grouped = preprocData$data.grouped,
                                      data.grouped.evenDim = preprocData$data.grouped.evenDim,
                                      fun_list = fun_list,
                                      g.interactome = g.interactome,
                                      quantile_critical_nodes = quantile_critical_nodes,
                                      violins = violins, ... = args_list)

  message("computing functional analysis - ", Sys.time())
  functionalAnalysis <- functional_analysis(LDA_pairw.results = quantitativeAnalysis$LDA_pairw.results,
                                            Unweighted_CN     = networkAnalysis$Unweighted_criticalNodes,
                                            Weighted_CN       = networkAnalysis$Weighted_criticalNodes,
                                            names_of_groups,tax_ID, categories)



  return(list(descriptiveStatistics = descriptiveStatistics,
              quantitativeAnalysis = quantitativeAnalysis,
              networkAnalysis      = networkAnalysis,
              functionalAnalysis   = functionalAnalysis
              ))
}
