#' filter_quantiles
#'
#' @param centralities data.frame of centralities to filter
#' @param orderBy integer or name of the centrality corresponding to the
#'     centrality position in 'colnames(centralities)' by which the data.frame rows are ordered.
#'     Default as 'rownames'
#' @param quantiles quantile to filter
#'
#' @importFrom stats quantile
#'
#' @return A data.frame filtered
#' @export
#'
filter_quantiles <- function(centralities,quantiles,orderBy = NULL){
  q.centralities <- apply(centralities,2, function(x){
    q.centr <- quantile(x,quantiles)
    names(which(x >= q.centr))
  },simplify = F)

  commonGenes <- Reduce(intersect, q.centralities)

  centralities.filtered <- centralities[commonGenes,]


  if (!nrow(centralities.filtered)){
    message("No genes meet the condition imposed, try changing qunatiles")
  } else {
    if (is.numeric(orderBy)){
      if (length(orderBy) == 1){
        if (orderBy > 0 & orderBy <= ncol(centralities)){
          ordering <- order(centralities.filtered[,orderBy], decreasing = T)
        } else {
          message("orderBy not recgnized, ordering by 'rownames'")
          ordering <- order(rownames(centralities.filtered))
        }
      } else {
        message("orderBy not recgnized, ordering by 'rownames'")
        ordering <- order(rownames(centralities.filtered))
      }
    } else if (is.character(orderBy)){
      if (length(orderBy) == 1){
        if (orderBy %in% colnames(centralities)){
          ordering <- order(centralities.filtered[,orderBy], decreasing = T)
        } else {
          message("orderBy not recgnized, ordering by 'rownames'")
          ordering <- order(rownames(centralities.filtered))
        }
      } else {
        message("orderBy not recgnized, ordering by 'rownames'")
        ordering <- order(rownames(centralities.filtered))
      }
    } else {
      ordering <- order(rownames(centralities.filtered))
    }

    centralities.filtered  <- centralities.filtered[ordering,]
  }
}
