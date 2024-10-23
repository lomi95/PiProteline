# test_that("functional_analysis performs functional analysis correctly", {
#   LDA_pairw.results <- list(
#     Comparison1 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene1", "Gene2"), DOWN = c("Gene3"))))
#       )
#     )
#   )
#   Unweighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene1 = 1))))
#   Weighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene2 = 1))))
#   names_of_groups <- c("Group1", "Group2")
#   tax_ID <- 9606
#   categories <- c("GO", "KEGG")
#
#   result <- functional_analysis(LDA_pairw.results, Unweighted_CN, Weighted_CN, names_of_groups, tax_ID, categories)
#
#   # Verificare che il risultato sia una lista
#   expect_type(result, "list")
#
#   # Controllare la presenza delle chiavi essenziali
#   expect_true(all(c("summaryTable", "enrLDA", "enrNetw", "enr_LDA.Netw") %in% names(result)))
#
#   # Verificare che i risultati di enrLDA e enrNetw siano presenti
#   expect_true("enr_LDA.diff" %in% names(result$enrLDA))
#   expect_true("enr_Netw.diff" %in% names(result$enrNetw))
# })
#
# test_that("functional_analysis handles empty input gracefully", {
#   LDA_pairw.results <- list()
#   Unweighted_CN <- list()
#   Weighted_CN <- list()
#   names_of_groups <- c("Group1", "Group2")
#   tax_ID <- 9606
#   categories <- c("GO", "KEGG")
#
#   result <- functional_analysis(LDA_pairw.results, Unweighted_CN, Weighted_CN, names_of_groups, tax_ID, categories)
#
#   # Verificare che il risultato sia una lista
#   expect_type(result, "list")
#
#   # Controllare che gli elenchi vuoti siano gestiti senza errori
#   expect_true(is.null(result$enrLDA$enr_LDA.diff))
#   expect_true(is.null(result$enrNetw$enr_Netw.diff))
# })
#
# test_that("functional_analysis computes enrichment correctly for multiple groups", {
#   LDA_pairw.results <- list(
#     Comparison1 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene1", "Gene2"), DOWN = c("Gene3"))))
#       )
#     ),
#     Comparison2 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene4"), DOWN = c("Gene5"))))
#       )
#     )
#   )
#   Unweighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene1 = 1), Group2 = data.frame(Gene3 = 1))))
#   Weighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene2 = 1), Group2 = data.frame(Gene5 = 1))))
#   names_of_groups <- c("Group1", "Group2")
#   tax_ID <- 9606
#   categories <- c("GO", "KEGG")
#
#   result <- functional_analysis(LDA_pairw.results, Unweighted_CN, Weighted_CN, names_of_groups, tax_ID, categories)
#
#   # Verificare che il risultato contenga arricchimenti per entrambi i gruppi
#   expect_true(length(result$enrLDA$enr_LDA.diff) > 0)
#   expect_true(length(result$enrNetw$enr_Netw.diff) > 0)
# })
#
# test_that("functional_analysis returns intersecting enrichment terms", {
#   LDA_pairw.results <- list(
#     Comparison1 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene1"), DOWN = c("Gene3"))))
#       )
#     )
#   )
#   Unweighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene1 = 1))))
#   Weighted_CN <- list(SearchMode1 = list(Hubs = list(Group1 = data.frame(Gene3 = 1))))
#   names_of_groups <- c("Group1")
#   tax_ID <- 9606
#   categories <- c("GO")
#
#   result <- functional_analysis(LDA_pairw.results, Unweighted_CN, Weighted_CN, names_of_groups, tax_ID, categories)
#
#   # Verificare che i termini di arricchimento incrociati siano calcolati correttamente
#   expect_true(length(result$enr_LDA.Netw$enrichment_SameTrend) > 0)
#   expect_true(length(result$enr_LDA.Netw$enrichment_OppositeTrend) > 0)
# })
