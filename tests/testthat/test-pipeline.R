# dataset <- openxlsx::read.xlsx("C:/Users/WKS/Desktop/Cardiac Amyloidosis/Files/C vs ATTR vs AL_norm.xlsx")
# names_of_groups <- c("Control","Allambda","ATTR")
# gene_column = 1
# g.interactome <- igraph::graph_from_edgelist(as.matrix(CoPPIs::interactome.hs[,3:4]), directed = F)
#
# library(igraph)
# test_that("pipeline works with minimal input", {
#   # Create a simple dataset
#   mock.dataset <- data.frame(matrix(abs(rnorm(1000)), nrow = 10))
#   colnames(mock.dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3", "treatment_3", "control_4", "control_5", "treatment_4", "treatment_5")
#   mock.dataset$GeneName <- sample(names(V(g.interactome)),nrow(mock.dataset))
#   # Call the pipeline
#   result <- pipeline(dataset = mock.dataset, names_of_groups = c("control", "treatment"),
#                      gene.column = ncol(mock.dataset), g.interactome = g.interactome,violins = F)
#
#   # Check if the result is a list
#   expect_type(result, "list")
#
#   # Check if the result contains the expected top-level elements
#   expect_true(all(c("Descriptive_Statistics", "Quantitative_Analysis", "Network_Analysis", "Functional_Analysis") %in% names(result)))
# })
#
