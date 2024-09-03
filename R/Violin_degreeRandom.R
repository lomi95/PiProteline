#' Violin_degreeRandom
#'
#' @param graph A igraph object
#' @param nIter number of random iterations
#' @param set_seed random seed
#'
#' @importFrom igraph degree
#' @importFrom igraph V
#' @importFrom igraph sample_gnp
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 annotate
#'
#'
#'
#' @return A ggplot object containing a distribution degree violin plot
#' @export
#'
Violin_degreeRandom <- function(graph, nIter = 100, set_seed = 1){
  set.seed(set_seed)
  ## We get the average degree of the graph, so we can create random networks with
  # each node having probability to have new edges 1/degree each time we assign a
  # new random edge


  Deg <- mean(igraph::degree(graph))
  prob <- 1/Deg
  L <- length(V(graph))
  randomGraphs <- lapply(1:nIter, function(x){
    sample_gnp(L, prob, directed=F, loops=FALSE)
  })

  Degree.randoms <- data.frame(mean_random_degree =
                                 sapply(randomGraphs, function(x) mean(igraph::degree(x))))

  # We plot the average random network degrees in a violin plot
  y.shift <- (max(Degree.randoms$mean_random_degree, Deg) - min(max(Degree.randoms$mean_random_degree), Deg))/40
  Violin.degree <- ggplot(Degree.randoms, aes(x = "Average Degree Random Networks", y = mean_random_degree)) +
    geom_violin()+
    geom_hline(yintercept = mean(Deg),linetype = "dashed", color = "red") +
    annotate("text", x = 1, y = Deg + y.shift, label = "Average Degree real network", color = "red")


  return(Violin.degree)
}


