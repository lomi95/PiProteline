#' Summary Table for Network Analysis Results
#'
#' This function generates a summary table that merges results from pairwise LDA, unweighted network critical nodes, and weighted network critical nodes. It calculates a combined score for each gene based on various metrics.
#'
#' @param volcano_plots The results from pairwise LDA analysis.
#' @param CN.unweighted The list of critical nodes identified in unweighted networks.
#' @param CN.weighted The list of critical nodes identified in weighted networks.
#'
#' @return A list containing:
#' \describe{
#'   \item{summaryLDA}{A data frame summarizing the LDA analysis results.}
#'   \item{summaryUnweightedNetw}{A data frame summarizing the critical nodes identified in unweighted networks.}
#'   \item{summaryWeightedNetw}{A data frame summarizing the critical nodes identified in weighted networks.}
#'   \item{summary}{A merged data frame containing the combined summary table with the following columns:}
#'   \itemize{
#'     \item{GeneName:} The name of the gene.
#'     \item{Various metrics:} Metrics derived from LDA results, unweighted, and weighted networks.
#'     \item{rawSum:} The sum of non-missing values across all metrics.
#'     \item{combinedScore:} A score calculated based on different metrics with weighted contributions.
#'   }
#' }
#' @export
#'
#' @examples
#' # Example usage
#' \dontrun{
#' LDA_results <- list(gene_matrix = data.frame(GeneName = c("Gene1", "Gene2"),
#'                         LDA_Score = c(5, 3)))
#' CN_unweighted <- list(CN.M = data.frame(GeneName = c("Gene1", "Gene3"),
#'                           Specific_Centrality_unweighted = c(2, 4)))
#' CN_weighted <- list(CN.M = data.frame(GeneName = c("Gene1", "Gene2"),
#'                         Centrality_weighted = c(3, 6)))
#'
#' summary_table(volcano_plots = LDA_results, CN.unweighted = CN_unweighted,
#'                     CN.weighted = CN_weighted)
#' }
summary_table <- function(volcano_plots, CN.unweighted, CN.weighted) {
  # Calcolare i riassunti per LDA, rete non pesata e pesata
  summaryLDA    <- summary_LDA(volcano_plots)
  summaryUnwNet <- summary_CriticalNodes(CN.unweighted, "_unweighted")
  summaryWNet   <- summary_CriticalNodes(CN.weighted, "_weighted")

  # Unire i risultati
  Merge1 <- merge(summaryLDA$gene_matrix, summaryUnwNet$CN.M, by = "GeneName", all = TRUE)
  Merge2 <- merge(Merge1, summaryWNet$CN.M, by = "GeneName", all = TRUE)

  # Calcolare rawSum e combinedScore
  Merge2$rawSum <- rowSums(Merge2[, -1], na.rm = TRUE)
  Merge2$combinedScore <- rowSums(Merge2[, grep("LDA", colnames(Merge2))], na.rm = TRUE) * 8 +
    rowSums(Merge2[, startsWith("Specific", colnames(Merge2))], na.rm = TRUE) * 2 +
    rowSums(Merge2[, grep("Centrality", colnames(Merge2))], na.rm = TRUE) * 2 +
    rowSums(Merge2[, grep("weighted", colnames(Merge2))], na.rm = TRUE) * 2

  # Ordinare i risultati per punteggio combinato
  Summary <- Merge2 %>% dplyr::arrange(dplyr::desc(combinedScore))

  # Restituire i riassunti
  return(list(
    summaryLDA = summaryLDA,
    summaryUnweightedNetw = summaryUnwNet,
    summaryWeightedNetw = summaryWNet,
    summary = Summary
  ))
}
