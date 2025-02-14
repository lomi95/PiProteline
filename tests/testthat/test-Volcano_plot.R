# test_that("volcano_plot correctly identifies up and down-regulated genes", {
#   example_data <- data.frame(
#     log2 = c(-2, -1.5, 0, 1.5, 2),
#     pv = -log10(c(0.01, 0.02, 0.5, 0.03, 0.001)),
#     gene = c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5")
#   )
#
#   result <- volcano_plot(example_data, fc_bounds = c(-1, 1), thr_pvalue = 0.05)
#
#   # Check the plot is a ggplot object
#   expect_s3_class(result$plot, "ggplot")
#
#   # Check the number of up-regulated and down-regulated genes
#   expect_equal(length(result$Genes$UP), 2)
#   expect_equal(length(result$Genes$DOWN), 2)
# })
#
# test_that("volcano_plot returns a plot with expected aesthetics", {
#   example_data <- data.frame(
#     log2 = rnorm(100),
#     pv = -log10(runif(100)),
#     gene = paste0("Gene", 1:100)
#   )
#
#   result <- volcano_plot(example_data, fc_bounds = c(-1, 1), thr_pvalue = 0.05)
#
#   # Verify the labels in the plot title
#   expect_true(grepl("volcano plot - UP Group1 vs DOWN Group2", result$plot$labels$title))
#
#   # Verify x and y axis labels
#   expect_equal(result$plot$labels$x, "log2(FC)")
#   expect_equal(result$plot$labels$y, "-log10(p.adjusted)")
# })
#
# test_that("volcano_plot handles data with no significant genes", {
#   example_data <- data.frame(
#     log2 = c(0.5, -0.5, 0.2, -0.3),
#     pv = -log10(c(0.5, 0.6, 0.8, 0.9)),
#     gene = c("GeneA", "GeneB", "GeneC", "GeneD")
#   )
#
#   result <- volcano_plot(example_data, fc_bounds = c(-1, 1), thr_pvalue = 0.05)
#
#   # Check there are no up-regulated or down-regulated genes
#   expect_equal(length(result$Genes$UP), 0)
#   expect_equal(length(result$Genes$DOWN), 0)
# })
