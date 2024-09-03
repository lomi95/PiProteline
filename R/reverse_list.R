#' reverse_list
#'
#' @param originalList list of lists composed the same between each others
#'
#' @return A list with elements switched of hierarchy
#'
#'
reverse_list <- function(originalList){
  listNames <- names(originalList[[1]])
  names(listNames) <- listNames
  newList <- lapply(listNames, function(x){
    lapply(originalList, function(y) y[[x]])
  })

}
