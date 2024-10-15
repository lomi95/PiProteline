#' Check if Input is an igraph Object
#'
#' This helper function checks whether the input is an `igraph` object and raises
#' an error if it is not. This is useful for ensuring that functions working with
#' graph data structures only receive valid inputs.
#'
#' @param g The object to check. It is expected to be an `igraph` object.
#'
#' @return No return value. The function throws an error if `graph` is not an
#'     `igraph` object.
#'
#' @examples
#' \dontrun{
#'
#'
#' library(igraph)
#' g <- make_ring(10)
#' inherit_igraph(g)  # No error, valid igraph object
#'
#' inherit_igraph(1:10)  # Will throw an error
#' }
#'
inherit_igraph <- function(g) {
  if (!inherits(g, "igraph")) {
    stop("Input must be an igraph object.")
  }
}


