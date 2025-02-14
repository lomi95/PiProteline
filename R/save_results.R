#' Save Results to Excel Files
#'
#' This function saves the results PiProteline analyses to Excel files, organizing data into different sheets.
#'
#' @param quantitative_analysis_results Output of the `quantitative_analysis` function.
#' @param network_analysis_results Output of network analysis.
#' @param functional_analysis_results Output of functional analysis.
#' @param pipeline_results Output of the complete analysis pipeline. If provided, individual results (`quantitative_analysis_results`, `network_analysis_results`, `functional_analysis_results`) are ignored.
#' @param save_results_as Directory name where results will be saved. Defaults to "PiProteline_report".
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData writeDataTable saveWorkbook
#' @return Saves Excel files in the specified directory.
#' @export
#'
#' @examples
#' \dontrun{
#' save_results(quantitative_analysis_results, network_analysis_results,
#'              functional_analysis_results, save_results_as = "my_results")
#' }
save_results <- function(quantitative_analysis_results = NULL,
                         network_analysis_results = NULL,
                         functional_analysis_results = NULL,
                         pipeline_results = NULL,
                         save_results_as = "PiProteline_report"){

  if (is.null(pipeline_results)){
    if (any(is.null(c(quantitative_analysis_results,
                      network_analysis_results,
                      functional_analysis_results)))){
      stop("either pipeline output or all the others parameters should be given as input")
    }
    quantitativeAnalysis <- quantitative_analysis_results
    networkAnalysis <- network_analysis_results
    functionalAnalysis <- functional_analysis_results
  } else {
    quantitativeAnalysis <- pipeline_results$quantitativeAnalysis
    networkAnalysis <- pipeline_results$networkAnalysis
    functionalAnalysis <- pipeline_results$functionalAnalysis
  }

    if (dir.exists(save_results_as)) {
      counter <- 1
      while (dir.exists(paste0(save_results_as, "_", counter))) {
        counter <- counter + 1
      }
      save_results_as <- paste0(save_results_as, "_", counter)
      message("Directory already exists. Creating ", save_results_as)
    }
    dir.create(save_results_as)

    old_dir <- getwd()
    on.exit(setwd(old_dir), add = TRUE)
    setwd(save_results_as)

    # Saving summary ----------------------------------------------------------

    wb <- createWorkbook()

    addWorksheet(wb, "Summary")
    functionalAnalysis$summaryTable[isFALSE(functionalAnalysis$summaryTable)] <- NA
    writeDataTable(wb, "Summary", functionalAnalysis$summaryTable)

    addWorksheet(wb, "Quantitative Analysis")
    writeDataTable(wb, "Quantitative Analysis", quantitativeAnalysis$manova_results)

    addWorksheet(wb, "Pairwise Quantitative Analysis")
    lapply(seq_along(quantitativeAnalysis$manova_pairw_results), function(x){
      writeData(
        wb, "Pairwise Quantitative Analysis",
        names(quantitativeAnalysis$manova_pairw_results)[x],
        startCol = (x - 1) * (ncol(quantitativeAnalysis$manova_pairw_results[[x]]) + 1) + 1
      )
      writeDataTable(
        wb, "Pairwise Quantitative Analysis",
        quantitativeAnalysis$manova_pairw_results[[x]], startRow = 2,
        startCol = (x - 1) * (ncol(quantitativeAnalysis$manova_pairw_results[[x]]) + 1) + 1
      )
    })

    addWorksheet(wb, "Unweighted Network Analysis")
    lapply(seq_along(networkAnalysis$Unweighted_centralities), function(x){
      writeData(
        wb, "Unweighted Network Analysis",
        names(networkAnalysis$Unweighted_centralities)[x],
        startCol = (x - 1) * (ncol(networkAnalysis$Unweighted_centralities[[x]]) + 2) + 1
      )
      writeDataTable(
        wb, "Unweighted Network Analysis",
        networkAnalysis$Unweighted_centralities[[x]], startRow = 2, rowNames = T,
        startCol = (x - 1) * (ncol(networkAnalysis$Unweighted_centralities[[x]]) + 2) + 1
      )
    })

    addWorksheet(wb, "Weighted Network Analysis")
    lapply(seq_along(networkAnalysis$Weighted_centralities), function(x){
      writeData(
        wb, "Weighted Network Analysis",
        names(networkAnalysis$Weighted_centralities)[x],
        startCol = (x - 1) * (ncol(networkAnalysis$Weighted_centralities[[x]]) + 2) + 1
      )
      writeDataTable(
        wb, "Weighted Network Analysis",
        networkAnalysis$Weighted_centralities[[x]], startRow = 2, rowNames = T,
        startCol = (x - 1) * (ncol(networkAnalysis$Weighted_centralities[[x]]) + 2) + 1
      )
    })
    saveWorkbook(wb, paste0(save_results_as,"_summary.xlsx"))

    # Saving enrichment -------------------------------------------------------

    wb_enr <- createWorkbook()
    addWorksheet(wb_enr, "Single Profile Enrichment")
    writeDataTable(wb_enr, "Single Profile Enrichment", functionalAnalysis$singleProfileEnrichment)

    merged_enrichment_manova <- merged_enrichment(enr_list = functionalAnalysis$enrmanova$enr_manova)
    merged_enrichment_Netw <- merged_enrichment(enr_list = functionalAnalysis$enrNetw$enr_Netw)
    wb_enr <- dfList_on_excel(df_list = merged_enrichment_manova, wb = wb_enr,name_sheets =  "_DAPs",rowNames = F)
    wb_enr <- dfList_on_excel(merged_enrichment_Netw, wb_enr, "_CNs",rowNames = F)

    saveWorkbook(wb_enr, paste0(save_results_as,"_enrichment.xlsx"))


    # Saving trend enrichment -------------------------------------------------


    wb_enr_trend <- createWorkbook()
    lapply(names(functionalAnalysis$enr_manovanetw), function(x){
      addWorksheet(wb_enr_trend, x)
      writeDataTable(wb_enr_trend, x, functionalAnalysis$enr_manovanetw[[x]])

    })
    saveWorkbook(wb_enr_trend, paste0(save_results_as,"_enrichmentTrend.xlsx"))
    setwd("..")
}
