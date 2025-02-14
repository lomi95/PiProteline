set.seed(1)
library(igraph)
filteredInteractome <- filter_interactome(interactome_hs,scores_threshold = c("experimental" = 0.15,
                                                                              "database"    = 0.35))
g_interactome <- graph_from_edgelist(as.matrix(filteredInteractome[,3:4]), directed = F)

test_that("weighted_network_analysis performs basic analysis", {
  data_grouped <- list(
    Group1 = matrix(runif(2000), nrow = 200),
    Group2 = matrix(runif(2000), nrow = 200)
  )
  G <- sample(names(V(g_interactome)), 200)
  data_grouped <- lapply(data_grouped, function(x) {
    rownames(x) <- G
    return(x)
  })
  colnames(data_grouped$Group1) <- paste0("Group1_",1:10)
  colnames(data_grouped$Group2) <- paste0("Group2_",1:10)

  result <- weighted_network_analysis(data_grouped_even_dim = data_grouped, names_of_groups = c("Group1", "Group2"),
                                        fun_list = list(Degree = igraph::degree, Betweenness = igraph::betweenness),
                                        g_interactome = g_interactome,
                                        quantile_critical_nodes = 0.8)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the essential elements are present
  expect_true(all(c("PPI_correlations","Centralities", "CriticalNodes") %in% names(result)))
})

test_that("weighted_network_analysis handles data_unique when data_grouped is NULL", {
  data_unique <- matrix(runif(4000), nrow = 200)
  rownames(data_unique) <- sample(names(V(g_interactome)), nrow(data_unique))
  colnames(data_unique) <- c(paste0("Group1_",1:10),paste0("Group2_",1:10))
  result <- weighted_network_analysis(data_grouped = NULL, names_of_groups = c("Group1", "Group2"), data_unique = data_unique,
                                        fun_list = list(Degree = igraph::degree, Betweenness = igraph::betweenness),
                                        g_interactome = g_interactome,
                                        quantile_critical_nodes = 0.8)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the essential elements are present
  expect_true(all(c("PPI_correlations", "Centralities", "CriticalNodes") %in% names(result)))
})

test_that("weighted_network_analysis raises an error when both data_grouped and data_unique are NULL", {
  expect_error(weighted_network_analysis(data_grouped = NULL, names_of_groups = c("Group1", "Group2")),
               "Either 'data_unique' or 'data_grouped_even_dim' must be provided")
})

