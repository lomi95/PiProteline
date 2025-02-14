#' Summary of Critical Nodes
#'
#' This function generates a summary of critical nodes (Hubs and Bottlenecks) identified in network analysis results for multiple groups. It creates a matrix indicating the presence of each critical node across different groups and searching modes.
#'
#' @param output_CN A list containing the critical nodes output from the network analysis. The list is expected to have searching modes as top-level keys, followed by critical node types ("Hubs" or "Bottlenecks") and group names.
#' @param paste_colnames A character string to append to the column names of the resulting matrix. Default is an empty string.
#'
#' @return A list containing:
#' \describe{
#'   \item{prot_Netw}{A list where each element contains the critical nodes for a specific combination of searching mode, critical node type, and group.}
#'   \item{df_CN}{A data frame where rows represent unique critical nodes and columns indicate the presence of each node across various combinations of searching mode, critical node type, and group, with a final column for gene names.}
#' }
#' @export
#' @importFrom dplyr mutate relocate
#' @examples
#' \dontrun{
#' names_of_groups <- c("Group1", "Group2")
#'
#' fun_list <- c(betweenness, centroids, bridging_centrality)
#' preprocData <- preprocessing_data(dataset, names_of_groups,1, "TotSigNorm" )
#' UNA <- unweighted_network_analysis(names_of_groups = names_of_groups,
#'                                    data_unique = preprocData$data_unique,
#'                                    fun_list = fun_list)
#' WNA <- weighted_netowrk_analysis(data_grouped_even_dim = data_grouped_even_dim,
#'                                  names_of_groups = names_of_groups,
#'                                  fun_list = fun_list)
#' unweighted_CN <- UNA$CriticalNodes
#' weighted_CN   <- WNA$CriticalNodes
#'
#' summary_critical_nodes(unweighted_CN, "_unweighted")
#' summary_critical_nodes(weighted_CN, "_weighted")
#'
#' }
summary_critical_nodes <- function(output_CN, paste_colnames = "") {
  prot_Netw <- list()

  for (sm in names(output_CN)) {
    for (g in names(output_CN[[sm]])) {
      prot_Netw[[paste(sm, g, sep = "_")]] <- rownames(output_CN[[sm]][[g]])
    }
  }

  CN <- unique(unlist(prot_Netw))
  df_CN <- sapply(prot_Netw, function(g) CN %in% g)
  if (is.null(dim(df_CN))){
    df_CN <- data.frame(GeneName = character())
  } else {
    rownames(df_CN) <- CN
    colnames(df_CN) <- paste0(colnames(df_CN), paste_colnames)
    df_CN <- as.data.frame(df_CN) %>%
      dplyr::mutate(GeneName = rownames(df_CN)) %>%
      relocate(GeneName, .before = 1 )
  }

  return(list(prot_Netw = prot_Netw, df_CN = df_CN))
}
