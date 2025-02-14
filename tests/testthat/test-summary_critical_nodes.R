# test_that("summary_CriticalNodes extracts nodes correctly", {
#   CN.output <- list(
#     SearchMode1 = list(
#       Hubs = list(Group1 = data.frame(Gene1 = c(1), Gene2 = c(1))),
#       Bottlenecks = list(Group1 = data.frame(Gene3 = c(1)), Group2 = data.frame(Gene4 = c(1)))
#     ),
#     SearchMode2 = list(
#       Hubs = list(Group1 = data.frame(Gene2 = c(1)), Group2 = data.frame(Gene1 = c(1))),
#       Bottlenecks = list(Group1 = data.frame(Gene4 = c(1)))
#     )
#   )
#   names_of_groups <- c("Group1", "Group2")
#
#   result <- summary_CriticalNodes(CN.output, paste_colnames = "_CN")
#
#   # Verificare che il risultato sia una lista
#   expect_type(result, "list")
#
#   # Controllare la presenza delle chiavi essenziali
#   expect_true(all(c("prot_Netw", "CN.M") %in% names(result)))
#
#   # Verificare che i nodi critici siano estratti correttamente
#   expected_genes <- c("Gene1", "Gene2", "Gene3", "Gene4")
#   expect_true(all(expected_genes %in% rownames(result$CN.M)))
#
#   # Controllare se i nomi delle colonne della matrice di nodi sono corretti
#   expected_columns <- c("SearchMode1_Hubs_Group1_CN", "SearchMode1_Bottlenecks_Group1_CN",
#                         "SearchMode1_Bottlenecks_Group2_CN", "SearchMode2_Hubs_Group1_CN",
#                         "SearchMode2_Hubs_Group2_CN", "SearchMode2_Bottlenecks_Group1_CN")
#   expect_true(all(expected_columns %in% colnames(result$CN.M)))
# })
#
# test_that("summary_CriticalNodes handles missing groups gracefully", {
#   CN.output <- list(
#     SearchMode1 = list(
#       Hubs = list(Group1 = data.frame(Gene1 = c(1), Gene2 = c(1))),
#       Bottlenecks = list(Group1 = data.frame(Gene3 = c(1)))
#     ),
#     SearchMode2 = list(
#       Hubs = list(Group2 = data.frame(Gene1 = c(1)))
#     )
#   )
#   names_of_groups <- c("Group1", "Group2")
#
#   result <- summary_CriticalNodes(CN.output, paste_colnames = "_CN")
#
#   # Verificare che i nodi siano gestiti correttamente quando mancano gruppi
#   expect_true("SearchMode2_Hubs_Group1_CN" %in% colnames(result$CN.M))
#   expect_true(all(result$CN.M$SearchMode2_Hubs_Group1_CN == FALSE))
# })
#
# test_that("summary_CriticalNodes returns empty matrix for empty input", {
#   CN.output <- list()
#   names_of_groups <- c("Group1", "Group2")
#
#   result <- summary_CriticalNodes(CN.output, paste_colnames = "_CN")
#
#   # Verificare che la funzione restituisca una matrice vuota quando l'input è vuoto
#   expect_true(nrow(result$CN.M) == 0)
#   expect_true(ncol(result$CN.M) == 1) # Solo GeneName dovrebbe essere presente
# })
#
# test_that("summary_CriticalNodes processes multiple search modes correctly", {
#   CN.output <- list(
#     SearchMode1 = list(
#       Hubs = list(Group1 = data.frame(Gene1 = c(1)), Group2 = data.frame(Gene2 = c(1))),
#       Bottlenecks = list(Group1 = data.frame(Gene3 = c(1)))
#     ),
#     SearchMode2 = list(
#       Hubs = list(Group2 = data.frame(Gene1 = c(1))),
#       Bottlenecks = list(Group1 = data.frame(Gene4 = c(1)), Group2 = data.frame(Gene2 = c(1)))
#     )
#   )
#   names_of_groups <- c("Group1", "Group2")
#
#   result <- summary_CriticalNodes(CN.output, paste_colnames = "_CN")
#
#   # Verificare che la matrice contenga tutti i geni unici
#   expected_genes <- c("Gene1", "Gene2", "Gene3", "Gene4")
#   expect_true(all(expected_genes %in% rownames(result$CN.M)))
#
#   # Controllare che le colonne siano coerenti con le modalità di ricerca e i tipi di nodo
#   expected_columns <- c("SearchMode1_Hubs_Group1_CN", "SearchMode1_Hubs_Group2_CN",
#                         "SearchMode1_Bottlenecks_Group1_CN", "SearchMode2_Hubs_Group2_CN",
#                         "SearchMode2_Bottlenecks_Group1_CN", "SearchMode2_Bottlenecks_Group2_CN")
#   expect_equal(ncol(result$CN.M) - 1, length(expected_columns)) # Escluso GeneName
# })
