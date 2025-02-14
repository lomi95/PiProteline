test_that("centrality_quantiles works correctly with graph with unnamed nodes", {
  library(igraph)
  # Check names graph
  # Create a test graph
  g <- make_ring(10) %>%
    add_edges(c(1, 5, 2, 6))

  # Define some centrality functions to test
  fun_list <- list(Degree = degree,
                   Closeness = closeness)

  # Test with no additional arguments
  result <- centrality_quantiles(g, fun_list = fun_list)

  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the number of rows matches the number of common nodes
  Dg <- degree(g)
  Cg <- closeness(g)
  expect_equal(result, data.frame("Degree" = Dg,"Closeness" = Cg) %>% dplyr::arrange(Degree))

  # Check number of columns
  expect_equal(ncol(result),length(fun_list))
  expect_equal(colnames(result),names(fun_list))

  # Check that ordering works
  result_ordered <- centrality_quantiles(g, fun_list = fun_list, order_by = "Degree")
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))
  result_ordered <- centrality_quantiles(g, fun_list = fun_list, order_by = 1)
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))


  # Check that messages are correct
  expect_message(centrality_quantiles(g, fun_list = fun_list, order_by = "1egree"),"order_by not recognized, ordering by first column")


})


test_that("centrality_quantiles works correctly with named nodes", {

  library(igraph)
  # Create a test graph
  g <- make_ring(10) %>%
    add_edges(c(1, 5, 2, 6))
  vertex.attributes(g)$name <- paste0("Node_",1:10)

  # Define some centrality functions to test
  fun_list <- list(Degree = degree,
                   Closeness = closeness)

  # Test with no additional arguments
  result <- centrality_quantiles(g, fun_list = fun_list)

  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the number of rows matches the number of common nodes
  Dg <- degree(g)
  Cg <- closeness(g)
  expect_equal(result, data.frame("Degree" = Dg,"Closeness" = Cg)[order(vertex.attributes(g)$name),])

  # Check number of columns
  expect_equal(ncol(result),length(fun_list))
  expect_equal(colnames(result),names(fun_list))

  # Check that ordering works
  result_ordered <- centrality_quantiles(g, fun_list = fun_list, order_by = "Degree")
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))
  result_ordered <- centrality_quantiles(g, fun_list = fun_list, order_by = 1)
  expect_true(all(order(result_ordered$Degree, decreasing = T) == 1:nrow(result_ordered)))

  result_ordered <- suppressMessages(centrality_quantiles(g, fun_list = fun_list, order_by = "degree"))
  expect_true(all(order(rownames(result_ordered)) == 1:nrow(result_ordered)))
  result_ordered <- suppressMessages(centrality_quantiles(g, fun_list = fun_list, order_by = 0))
  expect_true(all(order(rownames(result_ordered)) == 1:nrow(result_ordered)))
  result_ordered <- suppressMessages(centrality_quantiles(g, fun_list = fun_list, order_by = length(fun_list)+1))
  expect_true(all(order(rownames(result_ordered)) == 1:nrow(result_ordered)))

  # Check that messages are correct
  expect_message(centrality_mean(g, fun_list = fun_list, order_by = "1egree"),"order_by not recognized, ordering by 'rownames'")

})
