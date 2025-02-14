#' Centrality Quantiles
#'
#' This function applies multiple centrality measures to a graph and filters nodes
#' based on whether their centrality score is above the quantile threshold specified.
#' It returns a data frame containing only the common nodes that meet this condition across
#' all centrality measures.
#'
#' You can choose to order the resulting data frame by a specific centrality measure
#' using the `order_by` argument.
#'
#' @param graph An `igraph` object representing the graph.
#' @param fun_list A list of centrality functions to apply. The functions should either
#'     return node centralities or edge centralities, but not a mix.
#' @param quantiles A threshold value representing the quantile of individual centrality
#'     measures for nodes. Nodes with a centrality score above the given quantile will
#'     be included. Default is 0 (includes all nodes).
#' @param order_by An integer or the name of the centrality by which to order the
#'     resulting data frame rows. If NULL, the data frame will be ordered by row names.
#'     Default is NULL.
#' @param ... Additional arguments to pass to the centrality functions. These will be
#'     filtered to match the parameters accepted by each function.
#'
#' @importFrom stats quantile
#' @importFrom igraph components delete_vertices
#' @return A data frame with nodes as row names and centrality measures as columns.
#'     Only nodes with a centrality score above the given quantile for each measure
#'     will be included.
#' @export
#'
#' @examples
#' library(igraph)
#' g <- make_ring(10)
#' centralities <- centrality_quantiles(g, fun_list = list(degree, closeness), quantiles = 0.5)
#' print(centralities)
#'
centrality_quantiles <- function(graph,
                                 fun_list,
                                 quantiles = 0,
                                 order_by = NULL,
                                 ...) {
  Args <- list(...)

  if (components(graph)$no > 1) {
    old_opt <- options()$warn
    options(warn = 1)
    warning("The graph has more than one component, this could give an error if
            'fun_list' applies to different component conditions")
    options(warn = old_opt)
  }

  centralities <- sapply(fun_list, function(fun) {
    # Get the function's parameters
    fun_params <- names(formals(fun))
    # Filter arguments to include only those accepted by the function
    filtered_args <- Args[names(Args) %in% fun_params]
    # Apply the function with the filtered arguments
    do.call(fun, c(list(graph), filtered_args))
  })

  # Find nodes with centrality above the quantile for each centrality measure
  q.centralities <- apply(centralities, 2, function(x) {
    q.centr <- quantile(x, quantiles)
    which(x >= q.centr)
  }, simplify = FALSE)

  # Find common nodes across all centrality measures
  commonGenes <- Reduce(intersect, q.centralities)

  centralities.filtered <- centralities[commonGenes, ]

  if (!is.null(rownames(centralities.filtered))){
    if (!nrow(centralities.filtered)) {
      message("No genes meet the condition imposed, try changing quantiles")
    } else {
      if (is.numeric(order_by)) {
        if (length(order_by) == 1) {
          if (order_by > 0 & order_by <= length(fun_list)) {
            ordering <- order(centralities.filtered[, order_by], decreasing = TRUE)
          } else {
            message("order_by not recognized, ordering by 'rownames'")
            ordering <- order(rownames(centralities.filtered))
          }
        } else {
          message("order_by not recognized, ordering by 'rownames'")
          ordering <- order(rownames(centralities.filtered))
        }
      } else if (is.character(order_by)) {
        if (length(order_by) == 1) {
          if (order_by %in% names(fun_list)) {
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
  } else {

    if (!nrow(centralities.filtered)) {
      message("No genes meet the condition imposed, try changing quantiles")
    } else {
      if (is.numeric(order_by)) {
        if (length(order_by) == 1) {
          if (order_by > 0 & order_by <= length(fun_list)) {
            ordering <- order(centralities.filtered[, order_by], decreasing = TRUE)
          } else {
            message("order_by not recognized, ordering by first column")
            ordering <- order(centralities.filtered[,1])
          }
        } else {
          message("order_by not recognized, ordering by first column")
          ordering <- order(centralities.filtered[,1])
        }
      } else if (is.character(order_by)) {
        if (length(order_by) == 1) {
          if (order_by %in% names(fun_list)) {
            ordering <- order(centralities.filtered[, order_by], decreasing = TRUE)
          } else {
            message("order_by not recognized, ordering by first column")
            ordering <- order(centralities.filtered[,1])
          }
        } else {
          message("order_by not recognized, ordering by first column")
          ordering <- order(centralities.filtered[,1])
        }
      } else {
        ordering <- order(centralities.filtered[,1])
      }

      centralities.filtered <- centralities.filtered[ordering, ]
    }
  }

  return(data.frame(centralities.filtered))
}
