#' Violin Plot of Average Betweenness Centrality Distribution for Random Graphs
#'
#' This function generates a violin plot showing the distribution of average betweenness centrality from a set of random graphs generated to preserve the degree sequence of the original graph, compared with the average betweenness of the given input graph.
#'
#' @param graph An `igraph` object representing the original network.
#' @param nIter An integer specifying the number of random iterations. Default is 100.
#' @param Weights A numeric vector representing edge weights (used as distances) for betweenness calculation. If `NULL`, unweighted betweenness is computed. Default is `NULL`.
#' @param set_seed An integer specifying the random seed for reproducibility. Default is 1.
#'
#' @importFrom igraph delete_vertices sample_degseq degree betweenness edge.attributes<-
#' @importFrom ggplot2 ggplot aes geom_violin geom_hline annotate
#'
#' @return A `ggplot` object containing a violin plot of the distribution of average betweenness centrality for the random graphs.
#' @examples
#' # Example usage
#' library(igraph)
#' g <- make_ring(10)
#' Violin_betweennessRandom(g, nIter = 100, set_seed = 42)
#'
#' @export
Violin_betweennessRandom <- function(graph, nIter = 100, Weights = NULL, set_seed = 1) {
  set.seed(set_seed)

  # Assign weights if provided
  if (!is.null(Weights)) {
    edge.attributes(graph)[[1]] <- Weights
  }

  # Remove vertices with zero degree
  graph.noZeros <- biggest_component(graph)

  # Update weights if needed
  if (!is.null(Weights)) {
    Weights <- edge.attributes(graph.noZeros)[[1]]
  }

  # Generate random graphs while preserving the degree sequence
  randomGraphs <- lapply(1:nIter, function(x) {
    sample_degseq(igraph::degree(graph.noZeros), method = "vl")
  })

  # Calculate average betweenness for each random graph
  Betweenness.randoms <- data.frame(mean_random_betweenness =
                                      sapply(randomGraphs, function(x) mean(betweenness(x, weights = Weights))))

  # Calculate average betweenness for the original graph
  meanBet <- mean(betweenness(graph.noZeros, weights = Weights))

  # Compute y-axis shift for annotation
  y.shift <- (max(Betweenness.randoms$mean_random_betweenness, meanBet) -
                min(max(Betweenness.randoms$mean_random_betweenness), meanBet)) / 40

  # Generate the violin plot
  Violin.betweenness <- ggplot(Betweenness.randoms, aes(x = "Average Betweenness Random Networks", y = mean_random_betweenness)) +
    geom_violin() +
    geom_hline(yintercept = meanBet, linetype = "dashed", color = "red") +
    annotate("text", x = 1, y = meanBet + y.shift, label = "Average Betweenness real network", color = "red")

  return(Violin.betweenness)
}
