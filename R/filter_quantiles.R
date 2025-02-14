#' Filter Centralities Based on Quantiles
#'
#' Filters a data frame of centrality measures by keeping only the rows (genes) that meet
#' the specified quantile thresholds across all centralities. Additionally, the resulting
#' data frame can be ordered based on a specified centrality.
#'
#' @param centralities A data frame where each column represents a centrality measure,
#'     and each row corresponds to a gene or network node.
#' @param quantiles Numeric vector specifying the quantile thresholds for filtering.
#'     Genes with values greater than or equal to the specified quantile for each centrality are retained.
#' @param order_by An integer or character specifying the centrality column to order the filtered data by.
#'     If an integer, it should be the index of the column; if a character, it should be the name of the column.
#'     If `NULL`, the rows will be ordered by row names. Default is `NULL`.
#'
#' @details The function calculates the specified quantile thresholds for each centrality column,
#'     filters the genes that satisfy these thresholds, and optionally orders the resulting data frame
#'     by the specified centrality column.
#'
#' @importFrom stats quantile
#'
#' @return A data frame of filtered centrality values, potentially ordered based on the specified column.
#'     If no genes meet the condition imposed, a message is displayed.
#' @examples
#' # Example centralities data
#' centralities <- data.frame(
#'   Betweenness = runif(100),
#'   Closeness = runif(100),
#'   Degree = runif(100)
#' )
#'
#' # Filter based on quantiles and order by Betweenness
#' filtered_result <- filter_quantiles(centralities, quantiles = 0.9, order_by = "Betweenness")
#' print(filtered_result)
#'
#' @export
filter_quantiles <- function(centralities, quantiles, order_by = NULL){
  q.centralities <- apply(centralities, 2, function(x){
    q.centr <- quantile(x, quantiles)
    names(which(x >= q.centr))
  }, simplify = FALSE)

  commonGenes <- Reduce(intersect, q.centralities)

  centralities.filtered <- centralities[commonGenes, ]

  if (!nrow(centralities.filtered)){
    message("No genes meet the condition imposed, try changing quantiles")
  } else {
    if (is.numeric(order_by)){
      if (length(order_by) == 1){
        if (order_by > 0 & order_by <= ncol(centralities)){
          ordering <- order(centralities.filtered[, order_by], decreasing = TRUE)
        } else {
          message("order_by not recognized, ordering by 'rownames'")
          ordering <- order(rownames(centralities.filtered))
        }
      } else {
        message("order_by not recognized, ordering by 'rownames'")
        ordering <- order(rownames(centralities.filtered))
      }
    } else if (is.character(order_by)){
      if (length(order_by) == 1){
        if (order_by %in% colnames(centralities)){
          ordering <- order(centralities.filtered[, order_by], decreasing = TRUE)
        } else {
          message("order_by not recognized, ordering by 'rownames'")
          ordering <- order(rownames(centralities.filtered))
        }
      } else {
        message("order_by not recognized, ordering by 'rownames'")
        ordering <- order(rownames(centralities.filtered))
      }
    } else {
      ordering <- order(rownames(centralities.filtered))
    }

    centralities.filtered <- centralities.filtered[ordering, ]
  }

  return(centralities.filtered)
}
