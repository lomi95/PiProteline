set.seed(1)
library(igraph)
filteredInteractome <- filter_interactome(interactome_hs,scores_threshold = c("experimental" = 0.15,
                                                                              "database"    = 0.35))
g.interactome <- graph_from_edgelist(as.matrix(filteredInteractome[,3:4]), directed = F)

test_that("weighted_network_analysis performs basic analysis", {
  data.grouped <- list(
    Group1 = matrix(runif(2000), nrow = 200),
    Group2 = matrix(runif(2000), nrow = 200)
  )
  G <- sample(names(V(g.interactome)), 200)
  data.grouped <- lapply(data.grouped, function(x) {
    rownames(x) <- G
    return(x)
  })
  colnames(data.grouped$Group1) <- paste0("Group1_",1:10)
  colnames(data.grouped$Group2) <- paste0("Group2_",1:10)

  result <- weighted_network_analysis(data.grouped.evenDim = data.grouped, names_of_groups = c("Group1", "Group2"),
                                        fun_list = list(Degree = igraph::degree, Betweenness = igraph::betweenness),
                                        g.interactome = g.interactome,
                                        quantile_critical_nodes = 0.8)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the essential elements are present
  expect_true(all(c("PPI_correlations","Centralities", "CriticalNodes") %in% names(result)))
})

test_that("weighted_network_analysis handles data.unique when data.grouped is NULL", {
  data.unique <- matrix(runif(4000), nrow = 200)
  rownames(data.unique) <- sample(names(V(g.interactome)), nrow(data.unique))
  colnames(data.unique) <- c(paste0("Group1_",1:10),paste0("Group2_",1:10))
  result <- weighted_network_analysis(data.grouped = NULL, names_of_groups = c("Group1", "Group2"), data.unique = data.unique,
                                        fun_list = list(Degree = igraph::degree, Betweenness = igraph::betweenness),
                                        g.interactome = g.interactome,
                                        quantile_critical_nodes = 0.8)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the essential elements are present
  expect_true(all(c("PPI_correlations", "Centralities", "CriticalNodes") %in% names(result)))
})

test_that("weighted_network_analysis raises an error when both data.grouped and data.unique are NULL", {
  expect_error(weighted_network_analysis(data.grouped = NULL, names_of_groups = c("Group1", "Group2")),
               "Either 'data.unique' or 'data.grouped' must be provided")
})

