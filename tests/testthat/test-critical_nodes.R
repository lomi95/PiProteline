test_that("critical_nodes returns correct subsets", {
  # Sample input data frame
  x <- data.frame(
    "group1_centrality1" = c(1, 0, 0, 1),
    "group1_centrality2" = c(1, 1, 0, 1),
    "group2_centrality1" = c(0, 1, 1, 1),
    "group2_centrality2" = c(1, 1, 1, 1),
    row.names = c("geneA", "geneB", "geneC", "geneD")
  )

  # Define groups and centralities
  groups <- c("group1", "group2")
  centralities <- c("centrality1", "centrality2")

  # Not specific search mode
  result_NS <- critical_nodes(x, groups, centralities, "")
  expect_equal(rownames(result_NS$group1), c("geneA","geneD"))
  expect_equal(rownames(result_NS$group2), c("geneB","geneC","geneD"))

  # Specific search mode: group1 should not have overlaps with group2
  result_specific <- critical_nodes(x, groups, centralities, "specific")

  expect_equal(rownames(result_specific$group1)[rowSums(result_specific$group1[,1:2])==2], "geneA")
  expect_equal(rownames(result_specific$group2)[rowSums(result_specific$group2[,3:4])==2], c("geneB","geneC"))

  # Centrality-specific search mode: group1 and group2 filtered by centralities
  result_cs <- critical_nodes(x, groups, centralities, "centralityspecific")

  expect_equal(nrow(result_cs$group1),0)
  expect_equal(rownames(result_cs$group2), "geneC")
})

test_that("critical_nodes handles non-matching groups", {
  # Sample input data frame
  x <- data.frame(
    "group1_centrality1" = c(1, 0, 1, 0),
    "group1_centrality2" = c(1, 1, 0, 0),
    "group2_centrality1" = c(0, 1, 1, 1),
    "group2_centrality2" = c(1, 1, 1, 0),
    row.names = c("geneA", "geneB", "geneC", "geneD")
  )

  # Define groups and centralities
  groups <- c("group1", "group3")  # group3 doesn't exist in the data
  centralities <- c("centrality1", "centrality2")

  result <- suppressWarnings(critical_nodes(x, groups, centralities, "specific"))

  # Should give a Warning
  # Expect a warning when calling the function with a non-existing group
  #expect_warning(critical_nodes(x, groups, centralities, "specific"), "group3 was not found in 'colnames(x)'")

  # group1 should return one result, group3 should be empty
  expect_equal(rownames(result$group1), "geneA")
  expect_equal(rownames(result$group3), character(0))  # No matches for group3
})
