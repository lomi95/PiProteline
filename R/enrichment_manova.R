#' Perform STRING Enrichment Analysis on manova Results
#'
#' This function performs STRING enrichment analysis on a list of proteins (manova results), filters the results based on specified categories, and can optionally compute the differences between enrichment terms in consecutive pairs of groups.
#'
#' @param prot_manova A list where each element contains a vector of protein identifiers (e.g., resulting from manova analysis).
#' @param tax_ID A numeric taxonomy ID (e.g., 9606 for Homo sapiens) to be used in the STRING enrichment analysis.
#' @param categories A character vector specifying the categories of enrichment terms to filter (e.g., "GO:BP", "KEGG").
#' @param differences Logical, if TRUE (default), computes the difference in enrichment terms between consecutive pairs of groups.
#'
#' @return If `differences = TRUE`, returns a list containing:
#' \item{enr_manova}{The original enrichment results for each group.}
#' \item{enr_manova.diff}{The differential enrichment terms between consecutive groups.}
#' If `differences = FALSE`, returns a list of enrichment results for each group.
#'
#' @details This function utilizes the STRING enrichment API via the `rbioapi::rba_string_enrichment` function to perform enrichment analysis for each group of proteins. It filters the enrichment results based on the specified categories and can optionally compute the difference in terms between consecutive pairs of groups.
#'
#' @examples
#' \dontrun{
#' # Example protein manova results
#' prot_manova <- list(c("P12345", "P67890"), c("P54321", "P09876"))
#' tax_ID <- 9606  # Homo sapiens
#' categories <- c("GO:BP", "KEGG")
#' enrichment_manova(prot_manova, tax_ID, categories)
#' }
#' @importFrom rbioapi rba_string_enrichment
#' @importFrom dplyr filter arrange
#' @importFrom magrittr %>%
#' @export
enrichment_manova <- function(prot_manova, tax_ID, categories, differences = T){

  enr_manova <- suppressMessages(lapply(prot_manova, function(x){
    if (length(x)){
      enr <- rbioapi::rba_string_enrichment(x, tax_ID, split_df = F)
    } else {
      enr <- rbioapi::rba_string_enrichment(c("RPL24","RPL26","RPL29","RPL34","RPS15","RPS24"), tax_ID, split_df = F)
      return(enr[0,])
    }
    if (!is.null(enr) & nrow(enr)){
      return(enr %>% dplyr::filter(category %in% categories))
    } else {
      enr <- rbioapi::rba_string_enrichment(c("RPL24","RPL26","RPL29","RPL34","RPS15","RPS24"), tax_ID, split_df = F)
      return(enr[0,])
    }
  }))

  if (differences == T){
    enr_manova.diff <- enr_manova
    if (length(enr_manova)){
      for (i in seq(1,length(enr_manova),by = 2)){
        enr_manova.diff[[i]]   <- enr_manova[[i]]   %>%
          dplyr::filter(term %in% setdiff(term,enr_manova[[i+1]]$term)) %>%
          dplyr::arrange(category, description)
        enr_manova.diff[[i+1]] <- enr_manova[[i+1]] %>%
          dplyr::filter(term %in% setdiff(term,enr_manova[[i]]$term)) %>%
          dplyr::arrange(category, description)
      }
    }

    return(list(enr_manova = enr_manova,
                enr_manova.diff = enr_manova.diff))
  } else {
    return(enr_manova)
  }
}
