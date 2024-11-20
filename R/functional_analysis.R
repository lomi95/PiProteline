#' Functional Analysis for Network and LDA Results
#'
#' This function performs a functional analysis combining LDA pairwise results and network critical nodes. It generates a summary table, performs enrichment analysis for LDA and network results, and identifies common enrichment terms.
#'
#' @param volcano_plots The results from pairwise LDA analysis.
#' @param Unweighted_CN The list of critical nodes identified in unweighted networks.
#' @param Weighted_CN The list of critical nodes identified in weighted networks.
#' @param names_of_groups A character vector specifying the names of the groups for analysis.
#' @param tax_ID An integer representing the taxonomy ID for the organism (e.g., 9606 for human).
#' @param categories A character vector specifying the enrichment categories to filter (e.g., "GO", "KEGG").
#' @param dataset A data.frame containing protein samples, with column named after names of groups
#' @param ... Additional arguments passed to \link{single_profile_enrichment} function.
#'
#' @return A list containing:
#' \describe{
#'   \item{summaryTable}{A list containing summary data for LDA results, unweighted, and weighted networks.}
#'   \item{enrLDA}{A list of enrichment results for LDA-derived gene sets.}
#'   \item{enrNetw}{A list containing enrichment results for network-derived gene sets, including differential enrichment.}
#'   \item{enr_LDA.Netw}{A list of intersecting enrichment terms between LDA and network results, for both same and opposite trends.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' volcano_plots <- list(
#'   Comparison1 = list(
#'     VolcanoPlots = list(
#'       list(list(Genes = list(UP = c("Gene1", "Gene2"), DOWN = c("Gene3"))))
#'     )
#'   )
#' )
#' Unweighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene1 = 1))))
#' Weighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene2 = 1))))
#' names_of_groups <- c("Group1", "Group2")
#' tax_ID <- 9606
#' categories <- c("GO", "KEGG")
#'
#' functional_analysis(volcano_plots, Unweighted_CN, Weighted_CN,
#'                     names_of_groups, tax_ID, categories)
#'
#' }
functional_analysis <- function(dataset, volcano_plots, Unweighted_CN, Weighted_CN,
                                names_of_groups, tax_ID, categories,...){

  summaryTable <- summary_table(volcano_plots = volcano_plots,
                                CN.unweighted = Unweighted_CN,
                                CN.weighted = Weighted_CN)


  enrLDA  <- enrichment_LDA(prot_LDA = summaryTable$summaryLDA$prot_LDA, tax_ID, categories)

  enrNetw <- enrichment_Netw(summaryTable$summaryUnweightedNetw$prot_Netw,
                             summaryTable$summaryWeightedNetw$prot_Netw,
                             names_of_groups,tax_ID,categories)
  names(enrLDA$enr_LDA.diff) <-  names(enrNetw$enr_Netw.diff)

  enr_LDA.Netw <- intersect_enrichment(enr1 = enrLDA$enr_LDA.diff,enr2 = enrNetw$enr_Netw.diff)

  args_SPE <- args_MDS <- args_list[intersect(names(args_list), names(formals(single_profile_enrichment)))]
  singleProfileEnrichment <- do.call(single_profile_enrichment,
                                     c(list(dataset = dataset,
                                            names_of_groups = names_of_groups,
                                            tax_ID = tax_ID,
                                            categories = categories,
                                            parallel = F),
                                       args_SPE)
  )


  return(list(summaryTable = summaryTable,
              enrLDA = enrLDA,
              enrNetw = enrNetw,
              enr_LDA.Netw = enr_LDA.Netw,
              singleProfileEnrichment))
}
