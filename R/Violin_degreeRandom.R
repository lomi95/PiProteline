#' Violin Plot of Average Degree Distribution for Random Graphs
#'
#' This function generates a violin plot showing the distribution of average degrees from a set of random graphs generated using the Erdős-Rényi model, compared with the average degree of a given input graph.
#'
#' @param graph An `igraph` object representing the original network.
#' @param nIter An integer specifying the number of random iterations. Default is 100.
#' @param set_seed An integer specifying the random seed for reproducibility. Default is 1.
#'
#' @importFrom igraph degree V sample_gnp
#' @importFrom ggplot2 ggplot aes geom_violin geom_hline annotate
#'
#' @return A `ggplot` object containing a violin plot of the distribution of average degrees from the random graphs.
#' @examples
#' # Example usage
#' library(igraph)
#' g <- make_ring(10)
#' Violin_degreeRandom(g, nIter = 100, set_seed = 42)
#'
#' @export
Violin_degreeRandom <- function(graph, nIter = 100, set_seed = 1) {
  set.seed(set_seed)

  # Calculate the average degree of the original graph
  Deg <- mean(igraph::degree(graph))
  prob <- 1 / Deg
  L <- length(V(graph))

  # Generate random graphs and calculate their average degrees
  randomGraphs <- lapply(1:nIter, function(x) {
    sample_gnp(L, prob, directed = FALSE, loops = FALSE)
  })

  Degree.randoms <- data.frame(mean_random_degree =
                                 sapply(randomGraphs, function(x) mean(igraph::degree(x))))

  # Plot the distribution of average degrees for the random graphs
  y.shift <- (max(Degree.randoms$mean_random_degree, Deg) - min(max(Degree.randoms$mean_random_degree), Deg)) / 40
  Violin.degree <- ggplot(Degree.randoms, aes(x = "Average Degree Random Networks", y = mean_random_degree)) +
    geom_violin() +
    geom_hline(yintercept = Deg, linetype = "dashed", color = "red") +
    annotate("text", x = 1, y = Deg + y.shift, label = "Average Degree real network", color = "red")

  return(Violin.degree)
}
