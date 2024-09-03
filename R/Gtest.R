#' G test
#'
#' @param list.groups List of groups that we want to test
#'
#' @importFrom utils combn
#' @return g values for each comparison
#' @export
#'
Gtest <- function(list.groups){
  couple.indexes <- combn(1:length(list.groups), 2)
  g.value <- Map(function(g1,g2)
    (2*g1*log(g1/((g1+g2)/2))+2*g2*log(g2/((g1+g2)/2)))*sign(g1-g2),
    couple.indexes[1, ], couple.indexes[2, ])
  return(g.value)
}
