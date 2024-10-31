#' Perform STRING Enrichment Analysis on LDA Results
#'
#' This function performs STRING enrichment analysis on a list of proteins (LDA results), filters the results based on specified categories, and can optionally compute the differences between enrichment terms in consecutive pairs of groups.
#'
#' @param prot_LDA A list where each element contains a vector of protein identifiers (e.g., resulting from LDA analysis).
#' @param tax_ID A numeric taxonomy ID (e.g., 9606 for Homo sapiens) to be used in the STRING enrichment analysis.
#' @param categories A character vector specifying the categories of enrichment terms to filter (e.g., "GO:BP", "KEGG").
#' @param diff Logical, if TRUE (default), computes the difference in enrichment terms between consecutive pairs of groups.
#'
#' @return If `diff = TRUE`, returns a list containing:
#' \item{enr_LDA}{The original enrichment results for each group.}
#' \item{enr_LDA.diff}{The differential enrichment terms between consecutive groups.}
#' If `diff = FALSE`, returns a list of enrichment results for each group.
#'
#' @details This function utilizes the STRING enrichment API via the `rbioapi::rba_string_enrichment` function to perform enrichment analysis for each group of proteins. It filters the enrichment results based on the specified categories and can optionally compute the difference in terms between consecutive pairs of groups.
#'
#' @examples
#' \dontrun{
#' # Example protein LDA results
#' prot_LDA <- list(c("P12345", "P67890"), c("P54321", "P09876"))
#' tax_ID <- 9606  # Homo sapiens
#' categories <- c("GO:BP", "KEGG")
#' enrichment_LDA(prot_LDA, tax_ID, categories)
#' }
#' @importFrom rbioapi rba_string_enrichment
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
enrichment_LDA <- function(prot_LDA, tax_ID, categories, diff = T){
  enr_LDA <- suppressMessages(lapply(prot_LDA, function(x){
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

  if (diff == T){
    enr_LDA.diff <- enr_LDA
    for (i in seq(1,length(enr_LDA),by = 2)){
      enr_LDA.diff[[i]]   <- enr_LDA[[i]]   %>% dplyr::filter(term %in% setdiff(term,enr_LDA[[i+1]]$term))
      enr_LDA.diff[[i+1]] <- enr_LDA[[i+1]] %>% dplyr::filter(term %in% setdiff(term,enr_LDA[[i]]$term))
    }

    return(list(enr_LDA = enr_LDA,
                enr_LDA.diff = enr_LDA.diff))
  } else {
    return(enr_LDA)
  }
}
