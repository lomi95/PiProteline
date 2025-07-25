#' Save Results to Excel Files
#'
#' This function saves the results PiProteline analyses to Excel files, organizing data into different sheets.
#'
#' @param quantitative_analysis_results Output of the `quantitative_analysis` function.
#' @param network_analysis_results Output of network analysis.
#' @param functional_analysis_results Output of functional analysis.
#' @param pipeline_results Output of the complete analysis pipeline. If provided, individual results (`quantitative_analysis_results`, `network_analysis_results`, `functional_analysis_results`) are ignored.
#' @param save_results_as Directory name where results will be saved. Defaults to "PiProteline_report".
#' @param filter_pvalue_or_fdr Character string specifying wether to filter by "p.value" or "p.adj".  Default is "p.value".
#' @param significance_threshold Numeric value specifying the significance threshold. Default is 0.05.
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
                         filter_pvalue_or_fdr = "p.value",
                         significance_threshold = 0.05,
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

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "Summary")
  functionalAnalysis$summaryTable[isFALSE(functionalAnalysis$summaryTable)] <- NA
  try(openxlsx::writeDataTable(wb, "Summary", functionalAnalysis$summaryTable))

  if (!is.null(quantitativeAnalysis$manova_results)){
    openxlsx::addWorksheet(wb, "Quantitative Analysis")
    try(openxlsx::writeDataTable(wb, "Quantitative Analysis", quantitativeAnalysis$manova_results))
  }

  openxlsx::addWorksheet(wb, "Pairwise Quantitative Analysis")
  lapply(seq_along(quantitativeAnalysis$manova_pairw_results), function(x){
    openxlsx::writeData(
      wb, "Pairwise Quantitative Analysis",
      names(quantitativeAnalysis$manova_pairw_results)[x],
      startCol = (x - 1) * (ncol(quantitativeAnalysis$manova_pairw_results[[x]]) + 1) + 1
    )
    try(openxlsx::writeDataTable(
      wb, "Pairwise Quantitative Analysis",
      quantitativeAnalysis$manova_pairw_results[[x]], startRow = 2,
      startCol = (x - 1) * (ncol(quantitativeAnalysis$manova_pairw_results[[x]]) + 1) + 1
    ))
  })

  openxlsx::addWorksheet(wb, "Unweighted Network Analysis")
  lapply(seq_along(networkAnalysis$Unweighted_centralities), function(x){
    openxlsx::writeData(
      wb, "Unweighted Network Analysis",
      names(networkAnalysis$Unweighted_centralities)[x],
      startCol = (x - 1) * (ncol(networkAnalysis$Unweighted_centralities[[x]]) + 2) + 1
    )
    try(openxlsx::writeDataTable(
      wb, "Unweighted Network Analysis",
      networkAnalysis$Unweighted_centralities[[x]], startRow = 2, rowNames = T,
      startCol = (x - 1) * (ncol(networkAnalysis$Unweighted_centralities[[x]]) + 2) + 1
    ))
  })

  openxlsx::addWorksheet(wb, "Weighted Network Analysis")
  lapply(seq_along(networkAnalysis$Weighted_centralities), function(x){
    openxlsx::writeData(
      wb, "Weighted Network Analysis",
      names(networkAnalysis$Weighted_centralities)[x],
      startCol = (x - 1) * (ncol(networkAnalysis$Weighted_centralities[[x]]) + 2) + 1
    )
    try(openxlsx::writeDataTable(
      wb, "Weighted Network Analysis",
      networkAnalysis$Weighted_centralities[[x]], startRow = 2, rowNames = T,
      startCol = (x - 1) * (ncol(networkAnalysis$Weighted_centralities[[x]]) + 2) + 1
    ))
  })
  openxlsx::saveWorkbook(wb, paste0(save_results_as,"_unfiltered_results.xlsx"))

# Saving significant results -------------------------------------------------

  wb_filtered <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_filtered, "generalQuantitativeAnalysis")


  qa_mr <- quantitativeAnalysis$manova_results
  if (is.null(qa_mr)){
    qa_mr <- quantitativeAnalysis$manova_pairw_results[[1]]
  }
  qa_mr_f <- filter_by_pvalue_fdr(df = qa_mr,
                                  filter_pvalue_or_fdr = filter_pvalue_or_fdr,
                                  significance_threshold =  significance_threshold)
  openxlsx::writeDataTable(wb_filtered, "generalQuantitativeAnalysis", qa_mr_f)

  qa_p_f <- lapply(quantitativeAnalysis$manova_pairw_results,
                   filter_by_pvalue_fdr,
                   filter_pvalue_or_fdr = filter_pvalue_or_fdr,
                   significance_threshold = significance_threshold)
  wb_filtered <- dfList_on_sheet(df_list = qa_p_f,
                                 wb = wb_filtered, name_sheet = "pairwise_qa", rowNames = F)

  cn_unweighted <- cn_centrality_table(centralities = networkAnalysis$Unweighted_centralities,
                                       cn_modes = networkAnalysis$Unweighted_criticalNodes)
  cn_weighted <- cn_centrality_table(centralities = networkAnalysis$Weighted_centralities,
                                     cn_modes = networkAnalysis$Weighted_criticalNodes)

  lapply(names(cn_unweighted), function(x){
    wb_filtered <- dfList_on_sheet(df_list = cn_unweighted[[x]],
                                   wb = wb_filtered, name_sheet = paste0("unweighted_",x), rowNames = T)
  })
  lapply(names(cn_unweighted), function(x){
    wb_filtered <- dfList_on_sheet(df_list = cn_weighted[[x]],
                                   wb = wb_filtered, name_sheet = paste0("weighted_",x), rowNames = T)
  })

  openxlsx::saveWorkbook(wb_filtered, paste0(save_results_as,"_significative_results.xlsx"))
  # Saving enrichment -------------------------------------------------------

  wb_enr <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_enr, "Single Profile Enrichment")
  spe <- functionalAnalysis$singleProfileEnrichment
  spe[,grep("enriched_in", colnames(spe))] <-
    lapply(spe[,grep("enriched_in", colnames(spe))], function(x) {
      ifelse(is.na(x), "", x)
    })

  try(openxlsx::writeDataTable(wb_enr, "Single Profile Enrichment", spe))

  merged_enrichment_manova <- merged_enrichment(enr_list = functionalAnalysis$enrmanova$enr_manova)
  merged_enrichment_Netw <- merged_enrichment(enr_list = functionalAnalysis$enrNetw$enr_Netw)
  wb_enr <- dfList_on_excel(df_list = merged_enrichment_manova, wb = wb_enr,name_sheets =  "_DAPs",rowNames = F)
  wb_enr <- dfList_on_excel(merged_enrichment_Netw, wb_enr, "_CNs",rowNames = F)

  openxlsx::saveWorkbook(wb_enr, paste0(save_results_as,"_enrichment.xlsx"))


  # Saving trend enrichment -------------------------------------------------


  wb_enr_trend <- openxlsx::createWorkbook()
  lapply(names(functionalAnalysis$enr_manovanetw), function(x){
    openxlsx::addWorksheet(wb_enr_trend, x)
    try(openxlsx::writeDataTable(wb_enr_trend, x, functionalAnalysis$enr_manovanetw[[x]]))

  })
  openxlsx::saveWorkbook(wb_enr_trend, paste0(save_results_as,"_enrichmentTrend.xlsx"))


  save_images(quantitativeAnalysis)
  setwd("..")

  lapply(c("manova_enrichment_plots",
           "network_enrichment_plots",
           "singleProfiles_enrichment_plots"), function(x){
    if (dir.exists(x)) {
      file.rename(from = x, to = paste0(save_results_as, "/", x))
    }
  })

}
