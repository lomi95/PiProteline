test_that("centrality_mean works correctly with graph with unnamed nodes", {

  # Check names graph
  # Create a test graph
  g <- igraph::make_ring(10) %>%
    igraph::add_edges(c(1, 5, 2, 6))

  # Define some centrality functions to test
  fun_list <- list(Degree = igraph::degree,
                   Closeness = igraph::closeness)

  # Test with no additional arguments
  result <- centrality_mean(g, fun_list = fun_list)

  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the number of rows matches the number of common nodes
  Dg <- igraph::degree(g)
  Cg <- igraph::closeness(g)
  expect_equal(result, data.frame("Degree"    = Dg[Dg>=mean(Dg)],
                                  "Closeness" = Cg[Cg>= mean(Cg)]))

  # Check number of columns
  expect_equal(ncol(result),length(fun_list))
  expect_equal(colnames(result),names(fun_list))

  # Check that ordering works
  result_ordered <- centrality_mean(g, fun_list = fun_list, orderBy = "Degree")
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))
  result_ordered <- centrality_mean(g, fun_list = fun_list, orderBy = 1)
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))

  # Check that messages are correct
  expect_message(centrality_mean(g, fun_list = fun_list, orderBy = "1egree"),"orderBy not recognized, ordering by first column")
})



test_that("centrality_mean works correctly with graph with named nodes", {

  # Create a test graph
  g <- igraph::make_ring(10) %>%
    igraph::add_edges(c(1, 5, 2, 6))
  igraph::vertex.attributes(g)$name <- paste0("Node_",1:10)

  # Define some centrality functions to test
  fun_list <- list(Degree = igraph::degree,
                   Closeness = igraph::closeness)

  # Test with no additional arguments
  result <- centrality_mean(g, fun_list = fun_list)

  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the number of rows matches the number of common nodes
  Dg <- igraph::degree(g)
  Cg <- igraph::closeness(g)
  expect_equal(result, data.frame("Degree"    = Dg[Dg>=mean(Dg)],
                                  "Closeness" = Cg[Cg>= mean(Cg)]))

  # Check number of columns
  expect_equal(ncol(result),length(fun_list))
  expect_equal(colnames(result),names(fun_list))

  # Check that ordering works
  result_ordered <- centrality_mean(g, fun_list = fun_list, orderBy = "Degree")
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))
  result_ordered <- centrality_mean(g, fun_list = fun_list, orderBy = 1)
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))

  # Check that messages are correct
  expect_message(centrality_mean(g, fun_list = fun_list, orderBy = "1egree"),"orderBy not recognized, ordering by 'rownames'")
})
