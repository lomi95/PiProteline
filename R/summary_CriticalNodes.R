#' Summary of Critical Nodes
#'
#' This function generates a summary of critical nodes (Hubs and Bottlenecks) identified in network analysis results for multiple groups. It creates a matrix indicating the presence of each critical node across different groups and searching modes.
#'
#' @param CN.output A list containing the critical nodes output from the network analysis. The list is expected to have searching modes as top-level keys, followed by critical node types ("Hubs" or "Bottlenecks") and group names.
#' @param paste_colnames A character string to append to the column names of the resulting matrix. Default is an empty string.
#'
#' @return A list containing:
#' \describe{
#'   \item{prot_Netw}{A list where each element contains the critical nodes for a specific combination of searching mode, critical node type, and group.}
#'   \item{CN.M}{A data frame where rows represent unique critical nodes and columns indicate the presence of each node across various combinations of searching mode, critical node type, and group, with a final column for gene names.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' CN.output <- list(
#'   SearchMode1 = list(
#'     Hubs = list(Group1 = data.frame(Gene1 = c(1), Gene2 = c(1))),
#'     Bottlenecks = list(Group1 = data.frame(Gene3 = c(1)), Group2 = data.frame(Gene4 = c(1)))
#'   ),
#'   SearchMode2 = list(
#'     Hubs = list(Group1 = data.frame(Gene2 = c(1)), Group2 = data.frame(Gene1 = c(1))),
#'     Bottlenecks = list(Group1 = data.frame(Gene4 = c(1)))
#'   )
#' )
#' names_of_groups <- c("Group1", "Group2")
#' summary_CriticalNodes(CN.output, paste_colnames = "_CN")
#' }
summary_CriticalNodes <- function(CN.output, paste_colnames = "") {
  prot_Netw <- list()

  # Estrarre i nodi critici per ogni modalità di ricerca, tipo di nodo e gruppo
  for (sm in names(CN.output)) {
    for (cn in c("Hubs", "Bottlenecks")) {
      names_of_groups <- names(CN.output[[sm]][[cn]])
      for (g in names_of_groups) {
        prot_Netw[[paste(sm, cn, g, sep = "_")]] <- rownames(CN.output[[sm]][[cn]][[g]])
      }
    }
  }

  # Creare la matrice di nodi critici
  CN <- unique(unlist(prot_Netw))
  CN.M <- sapply(prot_Netw, function(g) CN %in% g)
  rownames(CN.M) <- CN
  colnames(CN.M) <- paste0(colnames(CN.M), paste_colnames)
  CN.M <- as.data.frame(CN.M) %>% dplyr::mutate(GeneName = rownames(CN.M))

  # Restituire i risultati
  return(list(prot_Netw = prot_Netw, CN.M = CN.M))
}
