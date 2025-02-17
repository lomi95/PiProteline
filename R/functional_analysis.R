#' Functional Analysis for Network and manova Results
#'
#' This function performs a functional analysis combining manova pairwise results and network critical nodes. It generates a summary table, performs enrichment analysis for manova and network results, and identifies common enrichment terms.
#'
#' @param manova_pairwise_results The results from pairwise manova analysis.
#' @param unweighted_CN The list of critical nodes identified in unweighted networks.
#' @param weighted_CN The list of critical nodes identified in weighted networks.
#' @param names_of_groups A character vector specifying the names of the groups for analysis.
#' @param tax_ID An integer representing the taxonomy ID for the organism (e.g., 9606 for human).
#' @param categories A character vector specifying the enrichment categories to filter (e.g., "GO", "KEGG").
#' @param dataset A data.frame containing protein samples, with column named after names of groups
#' @param ... Additional arguments passed to \link{single_profile_enrichment} function.
#'
#' @return A list containing:
#' \describe{
#'   \item{summaryTable}{A list containing summary data for manova results, unweighted, and weighted networks.}
#'   \item{enrmanova}{A list of enrichment results for manova-derived gene sets.}
#'   \item{enrNetw}{A list containing enrichment results for network-derived gene sets, including differential enrichment.}
#'   \item{enr_manova.Netw}{A list of intersecting enrichment terms between manova and network results, for both same and opposite trends.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
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
#' names_of_groups <- c("Group1", "Group2")
#' tax_ID <- 9606
#' categories <- c("Process", "KEGG", "WikiPathways")
#'
#' functional_analysis(manova_pairwise_results, unweighted_CN, weighted_CN,
#'                     names_of_groups, tax_ID, categories)
#'
#' }
functional_analysis <- function(dataset, manova_pairwise_results, unweighted_CN, weighted_CN,
                                names_of_groups, tax_ID, categories, ...){

  args_list <- list(...)
  summaryTable <- summary_table(manova_pairwise_results = manova_pairwise_results,
                                unweighted_CN = unweighted_CN,
                                weighted_CN = weighted_CN)



  enrmanova  <- tryCatch(enrichment_manova(prot_manova = summaryTable$summarymanova$prot_manova, tax_ID, categories),
                         error = function(e){
                           message("Error in enrichment_manova:")
                           message(e)
                           return(NULL)
                         }
  )
  enrNetw <- tryCatch(enrichment_Netw(summaryTable$summaryUnweightedNetw$prot_Netw,
                                      summaryTable$summaryWeightedNetw$prot_Netw,
                             names_of_groups,tax_ID,categories),
                      error = function(e){
                        message("Error in enrichment_Netw:")
                        message(e)
                        return(NULL)
                      }
  )
  if (!is.null(enrNetw)){
    names(enrmanova$enr_manova.diff) <- names(enrNetw$enr_Netw.diff)
    if (!is.null(enrmanova)){
      enr_manova.Netw <- intersect_enrichment(enr1 = enrmanova$enr_manova.diff,
                                              enr2 = enrNetw$enr_Netw.diff)
    } else {
      enr_manova.Netw <- NULL
    }
  } else {
    enr_manova.Netw <- NULL
  }



  args_SPE <- args_list[intersect(names(args_list), names(formals(single_profile_enrichment)))]

  singleProfileEnrichment <- tryCatch(do.call(single_profile_enrichment,
                                              c(list(dataset = dataset,
                                                     names_of_groups = names_of_groups,
                                                     tax_ID = tax_ID,
                                                     categories = categories,
                                                     parallel = T),
                                                args_SPE)),
                                      error = function(e){
                                        message("Error in single_profile_enrichment:")
                                        message(e)
                                        return(data.frame(GeneName = character()))
                                      }
  )


  return(list(summaryTable = summaryTable$summary,
              enrmanova = enrmanova,
              enrNetw = enrNetw,
              enr_manovanetw = enr_manova.Netw,
              singleProfileEnrichment = singleProfileEnrichment))
}
