#' Summary Table for Network Analysis Results
#'
#' This function generates a summary table that merges results from pairwise manova, unweighted network critical nodes, and weighted network critical nodes. It calculates a combined score for each gene based on various metrics.
#'
#' @param manova_pairwise_results The results from pairwise manova analysis.
#' @param unweighted_CN The list of critical nodes identified in unweighted networks.
#' @param weighted_CN The list of critical nodes identified in weighted networks.
#'
#' @return A list containing:
#' \describe{
#'   \item{summarymanova}{A data frame summarizing the manova analysis results.}
#'   \item{summaryUnweightedNetw}{A data frame summarizing the critical nodes identified in unweighted networks.}
#'   \item{summaryWeightedNetw}{A data frame summarizing the critical nodes identified in weighted networks.}
#'   \item{summary}{A merged data frame containing the combined summary table with the following columns:}
#'   \itemize{
#'     \item{GeneName:} The name of the gene.
#'     \item{Various metrics:} Metrics derived from manova results, unweighted, and weighted networks.
#'     \item{rawSum:} The sum of non-missing values across all metrics.
#'     \item{combinedScore:} A score calculated based on different metrics with weighted contributions.
#'   }
#' }
#' @export
#'
#' @examples
#' # Example usage
#' \dontrun{
#' names_of_groups <- c("Group1", "Group2")
#'
#' fun_list <- c(betweenness, centroids, bridging_centrality)
#' preprocData <- preprocessing_data(dataset, names_of_groups,1, "TotSigNorm" )
#' manova_pairwise_results <- manova_pairwise(preprocData$data_norm,names_of_groups, 1 )
#' UNA <- unweighted_network_analysis(names_of_groups = names_of_groups,
#'                                    data_unique = preprocData$data_unique,
#'                                    fun_list = fun_list)
#' WNA <- weighted_netowrk_analysis(data_grouped_even_dim = data_grouped_even_dim,
#'                                  names_of_groups = names_of_groups,
#'                                  fun_list = fun_list)
#' unweighted_CN <- UNA$CriticalNodes
#' weighted_CN   <- WNA$CriticalNodes
#'
#' summary_table(manova_results = manova_pairwise_results, unweighted_CN = unweighted_CN,
#'                     weighted_CN = weighted_CN)
#' }
summary_table <- function(manova_pairwise_results, unweighted_CN, weighted_CN) {
  # Calcolare i riassunti per manova, rete non pesata e pesata
  summarymanova <- summary_manova(manova_pairwise_results)
  summaryUnwNet <- summary_critical_nodes(unweighted_CN, "_unweighted")
  summaryWNet   <- summary_critical_nodes(weighted_CN, "_weighted")

  # Unire i risultati
  mergedSummary <- merge(summarymanova$summary_manova, summaryUnwNet$df_CN, by = "GeneName", all = TRUE) %>%
    merge(summaryWNet$df_CN, by = "GeneName", all = TRUE)

  # Calcolare rawSum e combinedScore
  mergedSummary$rawSum <- apply(mergedSummary[,-1, drop = F], 1, function(x) sum(!is.na(x) & x != F))

  coeff_manova     <- 8
  coeff_specific   <- 2
  coeff_centrality <- 2
  coeff_weighted   <- 2

  mergedSummary$combinedScore <- apply(mergedSummary[, grep("FC", colnames(mergedSummary)), drop = F], 1, function(x) sum(!is.na(x)) ) * coeff_manova +
    rowSums(mergedSummary[, startsWith(colnames(mergedSummary), "Specific"), drop = F], na.rm = TRUE) * coeff_specific +
    rowSums(mergedSummary[, grep("Centrality", colnames(mergedSummary)), drop = F], na.rm = TRUE) * coeff_centrality +
    rowSums(mergedSummary[, grep("weighted", colnames(mergedSummary)), drop = F], na.rm = TRUE) * coeff_weighted

  # Ordinare i risultati per punteggio combinato
  Summary <- mergedSummary %>% dplyr::arrange(dplyr::desc(combinedScore))
  Summary[-c(1:4)] <- lapply(Summary[-c(1:4)],function(x) {
    x[x == F] <- NA
    return(x)
  })

  # Restituire i riassunti
  return(list(
    summarymanova = summarymanova,
    summaryUnweightedNetw = summaryUnwNet,
    summaryWeightedNetw = summaryWNet,
    summary = Summary
  ))
}
