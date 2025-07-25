#' Extract centralities from critical nodes
#'
#' @param centralities centralities output from a `network_analysis` function
#' (`network_analysis`,`unweighted network_analysis`,`weighted network_analysis`).
#' @param cn_modes cn_modes output from a `network_analysis` function
#' (`network_analysis`,`unweighted network_analysis`,`weighted network_analysis`).
#'
#' @return a list of dataframes with the centralities of the critical nodes
#' @export
#'
#' @examples
#' \dontrun{
#'   na <- network_analysis(data_grouped, data_grouped_even_dim, )
#'   cn_centrality_table(centralities, cn_modes)
#' }
cn_centrality_table <- function(centralities, cn_modes){
  lapply(cn_modes, function(m){
    M <- names(m)
    names(M) <- M
    lapply(M, function(cn){
      centralities[[cn]][rownames(m[[cn]]),]
    })
  })
}
