#' Violin_betweennessRandom
#'
#' @param graph A igraph object
#' @param nIter number of random iterations
#' @param Weights weights (as distances) to use computing the betweenness
#' @param set_seed random seed
#'
#' @importFrom igraph delete_vertices
#' @importFrom igraph sample_degseq
#' @importFrom igraph degree
#' @importFrom igraph betweenness
#' @importFrom igraph edge.attributes<-
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 annotate
#'
#'
#'
#' @return A ggplot object containing a distribution betweenness violin plot
#' @export
#'
Violin_betweennessRandom <- function(graph, nIter = 100, Weights = NULL,set_seed = 1){

  ### We create random network with each node having the same degree of the
  # original network

  if (!is.null(Weights)){
    edge.attributes(graph)[[1]] <- Weights
  }

  # Before doing so we need to delete every node with degree == 0
  graph.noZeros <- biggest_component(graph)

  if (!is.null(Weights)){
    Weights <-  edge.attributes(graph.noZeros)[[1]]
  }

  #Now we can create the random networks
  randomGraphs <- lapply(1:nIter, function(x){
    sample_degseq(igraph::degree(graph.noZeros),method = "vl")
  })

  #We compute the average betweenness for each random networks and we compare with the
  # original one
  if (is.null(Weights)){
    Betweenness.randoms <- data.frame(mean_random_betweenness =
                                        sapply(randomGraphs, function(x) mean(betweenness(x, weights = NULL))))
  } else {
    Betweenness.randoms <- data.frame(mean_random_betweenness =
                                        sapply(randomGraphs, function(x) mean(betweenness(x, weights = Weights))))
  }

  # We plot the average random network betweennesses in a violin plot

  if (is.null(Weights)){
    meanBet <- mean(betweenness(graph.noZeros, weights = NULL))
  } else {
    meanBet <- mean(betweenness(graph.noZeros, weights = Weights))
  }
  y.shift <- (max(Betweenness.randoms$mean_random_betweenness, meanBet) -
                min(max(Betweenness.randoms$mean_random_betweenness), meanBet))/40


  Violin.betweenness <- ggplot(Betweenness.randoms, aes(x = "Average Betweenness Random Networks", y = mean_random_betweenness)) +
    geom_violin()+
    geom_hline(yintercept = meanBet,linetype = "dashed", color = "red") +
    annotate("text", x = 1, y = meanBet + y.shift, label = "Average Betweenness real network", color = "red")

}
