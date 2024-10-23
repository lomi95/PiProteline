# test_that("summary_table merges results correctly", {
#   LDA_pairwresults <- quantAnalysis$LDA_pairw.results
#   CN_unweighted <- NetwAnalysis$Unweighted_criticalNodes
#   CN_weighted <- NetwAnalysis$Weighted_criticalNodes
#   result <- summary_table(LDA_pairwresults,
#                           CN.unweighted = CN_unweighted,
#                           CN.weighted = CN_weighted)
#
#   # Controllare che il risultato sia una lista
#   expect_type(result, "list")
#
#   # Verificare la presenza delle chiavi essenziali
#   expect_true(all(c("summaryLDA", "summaryUnweightedNetw", "summaryWeightedNetw", "summary") %in% names(result)))
#
#   # Controllare che il sommario unito contenga le colonne essenziali
#   expect_equal(45, length(colnames(result$summary)))
#
#   # Verificare che il numero di righe corrisponda al numero di geni unici
#   expect_equal(nrow(result$summary), 1749)
# })
# #
# # test_that("summary_table calculates combinedScore correctly", {
# #   LDA_results <- list(gene_matrix = data.frame(GeneName = c("Gene1", "Gene2"), LDA_Score = c(5, 3)))
# #   CN_unweighted <- list(CN.M = data.frame(GeneName = c("Gene1", "Gene3"), Specific_Centrality_unweighted = c(2, 4)))
# #   CN_weighted <- list(CN.M = data.frame(GeneName = c("Gene1", "Gene2"), Centrality_weighted = c(3, 6)))
# #
# #   result <- summary_table(LDA_pairw.results = LDA_results, CN.unweighted = CN_unweighted, CN.weighted = CN_weighted)
# #
# #   # Verificare che il calcolo di combinedScore sia corretto
# #   expected_combinedScore_Gene1 <- 5 * 8 + 2 * 2 + 3 * 2
# #   expect_equal(result$summary$combinedScore[result$summary$GeneName == "Gene1"], expected_combinedScore_Gene1)
# # })
# #
# # test_that("summary_table handles missing data gracefully", {
# #   LDA_results <- list(gene_matrix = data.frame(GeneName = c("Gene1"), LDA_Score = c(5)))
# #   CN_unweighted <- list(CN.M = data.frame(GeneName = c("Gene2"), Specific_Centrality_unweighted = c(2)))
# #   CN_weighted <- list(CN.M = data.frame(GeneName = c("Gene3"), Centrality_weighted = c(3)))
# #
# #   result <- summary_table(LDA_pairw.results = LDA_results, CN.unweighted = CN_unweighted, CN.weighted = CN_weighted)
# #
# #   # Verificare che la funzione gestisca i dati mancanti senza errori
# #   expect_equal(nrow(result$summary), 3)
# #
# #   # Controllare se il combinedScore è calcolato come 0 quando tutte le colonne sono NA
# #   expect_true(all(is.na(result$summary$combinedScore[result$summary$GeneName == "Gene3"])))
# # })
