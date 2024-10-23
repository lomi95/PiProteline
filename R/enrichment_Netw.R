#' Network Enrichment Analysis
#'
#' This function performs enrichment analysis for protein groups from unweighted and weighted networks, using the STRING database. It identifies enrichment categories and computes differential enrichment between groups.
#'
#' @param prot_UnwNetw A list of protein sets derived from unweighted networks, grouped by specified categories.
#' @param prot_WNetw A list of protein sets derived from weighted networks, grouped by specified categories.
#' @param names_of_groups A character vector specifying the names of the groups to analyze.
#' @param tax_ID An integer representing the taxonomy ID for the organism (e.g., 9606 for human).
#' @param categories A character vector specifying the enrichment categories to filter (e.g., "GO", "KEGG").
#' @param diff A logical value indicating whether to calculate differential enrichment between groups. Default is `TRUE`.
#'
#' @return If `diff` is `TRUE`, a list containing:
#' \describe{
#'   \item{enr_Netw}{A list of enrichment results for each group.}
#'   \item{enr_Netw.diff}{A list of differential enrichment results between each pair of groups.}
#' }
#' If `diff` is `FALSE`, it returns a list of enrichment results for each group.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' prot_UnwNetw <- list(
#'   Group1 = c("P12345", "Q67890"),
#'   Group2 = c("P54321", "Q09876")
#' )
#' prot_WNetw <- list(
#'   Group1 = c("A12345", "B67890"),
#'   Group2 = c("A54321", "B09876")
#' )
#' names_of_groups <- c("Group1", "Group2")
#' tax_ID <- 9606
#' categories <- c("GO", "KEGG")
#'
#' enrichment_Netw(prot_UnwNetw, prot_WNetw, names_of_groups, tax_ID, categories, diff = TRUE)
#' }
enrichment_Netw <- function(prot_UnwNetw, prot_WNetw, names_of_groups, tax_ID, categories, diff = TRUE) {
  names(names_of_groups) <- names_of_groups
  enr_Net <- suppressMessages(lapply(names_of_groups, function(x) {
    # Unire i gruppi di proteine non pesate e pesate
    prot_group <- union(unlist(prot_UnwNetw[grep(x, names(prot_UnwNetw))]),
                        unlist(prot_WNetw[grep(x, names(prot_WNetw))]))
    # Eseguire l'arricchimento utilizzando la libreria rbioapi
    enr <- rbioapi::rba_string_enrichment(prot_group, tax_ID, split_df = FALSE)
    # Filtrare i risultati per le categorie specificate
    if (!is.null(enr)) {
      return(enr %>% dplyr::filter(category %in% categories))
    } else {
      # Restituire un dataframe vuoto se l'arricchimento non produce risultati
      enr <- rbioapi::rba_string_enrichment(c("RPL24", "RPL26", "RPL29", "RPL34", "RPS15", "RPS24"), tax_ID, split_df = FALSE)
      return(enr[0, ])
    }
  }))

  # Calcolare l'arricchimento differenziale se richiesto
  if (diff) {
    enr_Net.diff <- list()
    for (I in 1:(length(names_of_groups) - 1)) {
      for (J in (I + 1):length(names_of_groups)) {
        i <- names_of_groups[I]
        j <- names_of_groups[J]

        # Arricchimento specifico per ciascuna coppia di gruppi
        enr_Net.diff[[paste0(i, "_vs_", j)]] <- enr_Net[[i]] %>% dplyr::filter(term %in% setdiff(term, enr_Net[[j]]$term))
        enr_Net.diff[[paste0(j, "_vs_", i)]] <- enr_Net[[j]] %>% dplyr::filter(term %in% setdiff(term, enr_Net[[i]]$term))
      }
    }
    return(list(enr_Netw = enr_Net,
                enr_Netw.diff = enr_Net.diff))
  } else {
    return(enr_Netw)
  }
}
