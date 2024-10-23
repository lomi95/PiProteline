# test_that("enrichment_Netw performs enrichment analysis correctly", {
#   prot_UnwNetw <- list(
#     Group1 = c("P12345", "Q67890"),
#     Group2 = c("P54321", "Q09876")
#   )
#   prot_WNetw <- list(
#     Group1 = c("A12345", "B67890"),
#     Group2 = c("A54321", "B09876")
#   )
#   names_of_groups <- c("Group1", "Group2")
#   tax_ID <- 9606
#   categories <- c("GO", "KEGG")
#
#   result <- enrichment_Netw(prot_UnwNetw, prot_WNetw, names_of_groups, tax_ID, categories, diff = TRUE)
#
#   # Check if the output is a list
#   expect_type(result, "list")
#
#   # Verify that the list contains enr_Net and enr_Net.diff if diff is TRUE
#   expect_true(all(c("enr_Net", "enr_Net.diff") %in% names(result)))
# })
#
# test_that("enrichment_Netw handles no differential enrichment gracefully", {
#   prot_UnwNetw <- list(
#     Group1 = c("P12345"),
#     Group2 = c("Q67890")
#   )
#   prot_WNetw <- list(
#     Group1 = c("A12345"),
#     Group2 = c("B67890")
#   )
#   names_of_groups <- c("Group1", "Group2")
#   tax_ID <- 9606
#   categories <- c("GO")
#
#   result <- enrichment_Netw(prot_UnwNetw, prot_WNetw, names_of_groups, tax_ID, categories, diff = FALSE)
#
#   # Check if the output is a list and contains only enr_Net
#   expect_type(result, "list")
#   expect_true(!"enr_Net.diff" %in% names(result))
#   expect_true("enr_Net" %in% names(result))
# })
#
# test_that("enrichment_Netw returns empty data frame when enrichment fails", {
#   prot_UnwNetw <- list(Group1 = character(0), Group2 = character(0))
#   prot_WNetw <- list(Group1 = character(0), Group2 = character(0))
#   names_of_groups <- c("Group1", "Group2")
#   tax_ID <- 9606
#   categories <- c("GO")
#
#   result <- enrichment_Netw(prot_UnwNetw, prot_WNetw, names_of_groups, tax_ID, categories, diff = FALSE)
#
#   # Check if enr_Net contains empty data frames for each group
#   expect_true(all(sapply(result$enr_Net, function(x) nrow(x) == 0)))
# })
#
# test_that("enrichment_Netw computes differential enrichment correctly", {
#   prot_UnwNetw <- list(
#     Group1 = c("P12345", "Q67890"),
#     Group2 = c("P12345", "Q67890", "A12345")
#   )
#   prot_WNetw <- list(
#     Group1 = c("A12345"),
#     Group2 = c("B67890")
#   )
#   names_of_groups <- c("Group1", "Group2")
#   tax_ID <- 9606
#   categories <- c("GO")
#
#   result <- enrichment_Netw(prot_UnwNetw, prot_WNetw, names_of_groups, tax_ID, categories, diff = TRUE)
#
#   # Check if differential enrichment results are calculated for each pair of groups
#   expect_true(length(result$enr_Net.diff) > 0)
#   expect_true(all(sapply(result$enr_Net.diff, function(x) inherits(x, "data.frame"))))
# })
