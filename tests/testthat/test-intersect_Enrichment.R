# test_that("intersect_enrichment finds common terms for the same trend", {
#   enr1 <- list(
#     Group1 = data.frame(term = c("Term1", "Term2"), description = c("Desc1", "Desc2"), category = c("Cat1", "Cat2")),
#     Group2 = data.frame(term = c("Term3", "Term4"), description = c("Desc3", "Desc4"), category = c("Cat3", "Cat4"))
#   )
#   enr2 <- list(
#     Group1 = data.frame(term = c("Term1", "Term5"), description = c("Desc1", "Desc5"), category = c("Cat1", "Cat5")),
#     Group2 = data.frame(term = c("Term3", "Term6"), description = c("Desc3", "Desc6"), category = c("Cat3", "Cat6"))
#   )
#
#   result <- intersect_enrichment(enr1, enr2, oppositeTrend = FALSE)
#
#   # Verificare che il risultato sia una lista
#   expect_type(result, "list")
#
#   # Controllare se i termini comuni sono corretti
#   expect_true(all(result$Group1$term == "Term1"))
#   expect_true(all(result$Group2$term == "Term3"))
# })
#
# test_that("intersect_enrichment finds common terms with opposite trends", {
#   enr1 <- list(
#     Group1 = data.frame(term = c("Term1", "Term2"), description = c("Desc1", "Desc2"), category = c("Cat1", "Cat2")),
#     Group2 = data.frame(term = c("Term3", "Term4"), description = c("Desc3", "Desc4"), category = c("Cat3", "Cat4")),
#     Group3 = data.frame(term = c("Term5", "Term6"), description = c("Desc5", "Desc6"), category = c("Cat5", "Cat6")),
#     Group4 = data.frame(term = c("Term7", "Term8"), description = c("Desc7", "Desc8"), category = c("Cat7", "Cat8"))
#   )
#   enr2 <- list(
#     Group1 = data.frame(term = c("Term3", "Term2"), description = c("Desc3", "Desc2"), category = c("Cat3", "Cat2")),
#     Group2 = data.frame(term = c("Term1", "Term4"), description = c("Desc1", "Desc4"), category = c("Cat1", "Cat4")),
#     Group3 = data.frame(term = c("Term7", "Term6"), description = c("Desc7", "Desc6"), category = c("Cat7", "Cat6")),
#     Group4 = data.frame(term = c("Term5", "Term8"), description = c("Desc5", "Desc8"), category = c("Cat5", "Cat8"))
#   )
#
#   result <- intersect_enrichment(enr1, enr2, oppositeTrend = TRUE)
#
#   # Verificare che i termini con trend opposto siano trovati correttamente
#   expect_true(all(result$enrichment_OppositeTrend$Group1$term == "Term3"))
#   expect_true(all(result$enrichment_OppositeTrend$Group2$term == "Term1"))
# })
#
# test_that("intersect_enrichment returns warning if names differ", {
#   enr1 <- list(GroupA = data.frame(term = c("Term1"), description = c("Desc1"), category = c("Cat1")))
#   enr2 <- list(GroupB = data.frame(term = c("Term1"), description = c("Desc1"), category = c("Cat1")))
#
#   expect_warning(intersect_enrichment(enr1, enr2, oppositeTrend = FALSE), "names of enr1 and enr2 differ")
# })
#
# test_that("intersect_enrichment handles different lengths of enr1 and enr2", {
#   enr1 <- list(Group1 = data.frame(term = c("Term1"), description = c("Desc1"), category = c("Cat1")))
#   enr2 <- list()
#
#   expect_error(intersect_enrichment(enr1, enr2, oppositeTrend = TRUE), "enr1 and enr2 must have the same length")
# })
#
# test_that("intersect_enrichment handles empty input gracefully", {
#   enr1 <- list(Group1 = data.frame(term = character(), description = character(), category = character()))
#   enr2 <- list(Group1 = data.frame(term = character(), description = character(), category = character()))
#
#   result <- intersect_enrichment(enr1, enr2, oppositeTrend = FALSE)
#
#   # Verificare che la funzione restituisca una lista vuota
#   expect_true(length(result$Group1) == 0)
# })
