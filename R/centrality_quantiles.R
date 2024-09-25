#' centrality_quantiles
#'
#' @param fun_list list of centrality functions to apply.
#'     Use only nodes centralities or only edges centralities.
#' @param graph igraph object
#' @param quantiles threshold of individual centrality quantiles for nodes.
#'     If 0 it will gives all nodes centralities. Default = 0
#' @param ... evantual further inputs for centrality functions. All functions that
#'     accept a named parameter will accept it if given
#' @param orderBy integer or name of the centrality corresponding to the
#'     centrality position in 'fun_list' by which the data.frame rows are ordered.
#'     Default as 'rownames'
#'
#' @importFrom stats quantile
#' @importFrom igraph components
#' @importFrom igraph delete_vertices
#'
#'
#' @return A 'data.frame' with genes as 'rownames', and columns as centrality measures
#' @export
#'
centrality_quantiles <- function(graph,
                                 fun_list,
                                 quantiles = 0,
                                 orderBy = NULL,
                                 ...) {
  Args <- list(...)


  if (components(graph)$no>1){
    options(warn = 1)
    warning("The graph has more than one component, this could give an error if
            'fun_list' apply to different component conditions")
  }
  centralities <- sapply(fun_list, function(fun) {
    # Otteniamo i nomi dei parametri della funzione
    fun_params <- names(formals(fun))
    # Filtriamo gli argomenti opzionali in base a quelli che la funzione accetta
    filtered_args <- Args[names(Args) %in% fun_params]
    # Applichiamo la funzione al grafo con gli argomenti filtrati
    do.call(fun, c(list(graph), filtered_args))
  })

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
        if (orderBy > 0 & orderBy <= length(fun_list)){
          ordering <- order(centralities.filtered[,orderBy], decreasing = T)
        } else {
          message("orderBy not recognized, ordering by 'rownames'")
          ordering <- order(rownames(centralities.filtered))
        }
      } else {
        message("orderBy not recognized, ordering by 'rownames'")
        ordering <- order(rownames(centralities.filtered))
      }
    } else if (is.character(orderBy)){
      if (length(orderBy) == 1){
        if (orderBy %in% names(fun_list)){
          ordering <- order(centralities.filtered[,orderBy], decreasing = T)
        } else {
          message("orderBy not recognized, ordering by 'rownames'")
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
  return(data.frame(centralities.filtered))
}
