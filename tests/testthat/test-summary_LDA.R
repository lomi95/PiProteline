# test_that("summary_LDA extracts genes correctly", {
#   LDA_pairw.results <- list(
#     Comparison1 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene1", "Gene2"), DOWN = c("Gene3"))))
#       )
#     ),
#     Comparison2 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene3", "Gene4"), DOWN = c("Gene1"))))
#       )
#     )
#   )
#
#   result <- summary_LDA(LDA_pairw.results)
#
#   # Controllare che il risultato sia una lista
#   expect_type(result, "list")
#
#   # Verificare la presenza delle chiavi essenziali
#   expect_true(all(c("prot_LDA", "gene_matrix") %in% names(result)))
#
#   # Verificare che i geni siano estratti correttamente
#   expected_genes <- c("Gene1", "Gene2", "Gene3", "Gene4")
#   expect_true(all(expected_genes %in% rownames(result$gene_matrix)))
#
#   # Controllare se i nomi delle colonne della matrice di geni sono corretti
#   expected_columns <- c("Comparison1_UP_LDA", "Comparison1_DOWN_LDA", "Comparison2_UP_LDA", "Comparison2_DOWN_LDA")
#   expect_true(all(expected_columns %in% colnames(result$gene_matrix)))
# })
#
# test_that("summary_LDA handles missing up or down genes gracefully", {
#   LDA_pairw.results <- list(
#     Comparison1 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene1", "Gene2"), DOWN = character(0))))
#       )
#     ),
#     Comparison2 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = character(0), DOWN = c("Gene1", "Gene3"))))
#       )
#     )
#   )
#
#   result <- summary_LDA(LDA_pairw.results)
#
#   # Verificare che la funzione non restituisca errori quando i geni up o down sono assenti
#   expect_true(all(c("Gene1", "Gene2", "Gene3") %in% rownames(result$gene_matrix)))
#
#   # Controllare che i valori siano gestiti correttamente per i geni mancanti
#   expect_true(all(result$gene_matrix$Comparison1_DOWN_LDA[result$gene_matrix$GeneName == "Gene2"] == FALSE))
# })
#
# test_that("summary_LDA returns an empty matrix when input is empty", {
#   LDA_pairw.results <- list()
#
#   result <- summary_LDA(LDA_pairw.results)
#
#   # Verificare che la funzione restituisca una matrice vuota quando l'input è vuoto
#   expect_true(nrow(result$gene_matrix) == 0)
#   expect_true(ncol(result$gene_matrix) == 1) # Solo GeneName dovrebbe essere presente
# })
#
# test_that("summary_LDA processes multiple comparisons correctly", {
#   LDA_pairw.results <- list(
#     Comparison1 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene1", "Gene2"), DOWN = c("Gene3"))))
#       )
#     ),
#     Comparison2 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene3", "Gene4"), DOWN = c("Gene2"))))
#       )
#     ),
#     Comparison3 = list(
#       VolcanoPlots = list(
#         list(list(Genes = list(UP = c("Gene5"), DOWN = c("Gene1"))))
#       )
#     )
#   )
#
#   result <- summary_LDA(LDA_pairw.results)
#
#   # Verificare che la matrice dei geni contenga tutti i geni unici
#   expected_genes <- c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5")
#   expect_true(all(expected_genes %in% rownames(result$gene_matrix)))
#
#   # Controllare che i risultati siano coerenti con il numero di confronti
#   expect_equal(ncol(result$gene_matrix) - 1, 6) # Sei colonne per UP e DOWN
# })
