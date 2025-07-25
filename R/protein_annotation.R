#' Retrieve and Combine Protein Annotations from STRING
#'
#' @description
#' This function retrieves protein annotations from the STRING database through
#' its API, handles API limits by splitting requests into chunks, and combines
#' results into a consolidated dataframe.
#'
#' @param proteins_ids Character vector of protein identifiers (e.g., UniProt IDs)
#' @param tax_ID Numeric taxonomy identifier (e.g., 9606 for Homo sapiens)
#' @param splitting_API Numeric chunk size for API requests (default = 1999).
#'        Adjust based on API limits.
#' @param verbose Logical indicating whether to print progress messages
#'        (default = FALSE)
#'
#' @return A tibble with combined annotations containing:
#' \itemize{
#'   \item description: Annotation term description
#'   \item category: Annotation category
#'   \item term: Annotation term ID
#'   \item inputGenes: List of unique input gene identifiers
#'   \item preferredNames: List of unique preferred gene names
#'   \item number_of_genes: Count of unique genes per term
#' }
#'
#' @examples
#' \dontrun{
#'   # Example using human proteins
#'   protein_annotation(
#'     proteins_ids = c("P83731", "P61254", "Q9Y3D8", "P62249", "P62847", "P62851"),
#'     tax_ID = 9606
#'   )
#' }
#'
#' @export
#' @importFrom dplyr bind_rows group_by summarize ungroup
#' @importFrom rbioapi rba_string_annotations

protein_annotation <- function(proteins_ids, tax_ID, splitting_API = 1999, verbose = F){
  vect.ann <- seq(1,length(proteins_ids),by = splitting_API)

  #ping
  rbioapi::rba_string_annotations(c("RPL24","RPL26","RPL29","RPL34","RPS15","RPS24"), 9606, verbose = F)

  if (length(vect.ann)>1){
    all_annotation <- list()
    for (i in 1:(length(vect.ann)-1)){
      if (verbose) message("Annotating chunk ", i, " of ", length(vect.ann))
      all_annotation[[i]] <- rbioapi::rba_string_annotations(
        proteins_ids[vect.ann[i]:(vect.ann[i+1]-1)],tax_ID,verbose = F, split_df = F)
    }
    if (verbose) message("Annotating chunk ", i+1, " of ", length(vect.ann))
    all_annotation[[i+1]] <- rbioapi::rba_string_annotations(
      proteins_ids[vect.ann[i+1]:length(proteins_ids)],tax_ID, verbose = F, split_df = F)


    combined_df <- bind_rows(all_annotation) %>%
      group_by(description, category, term) %>%
      summarize(
        inputGenes = list(unique(unlist(inputGenes))),
        preferredNames = list(unique(unlist(preferredNames)))
      ) %>%
      ungroup()
    combined_df$number_of_genes <- sapply(combined_df$preferredNames, length)

  } else {
    combined_df <- rbioapi::rba_string_annotations(proteins_ids, tax_ID, verbose = F, split_df = F) %>%
      select(
        description,
        category,
        term,
        inputGenes,
        preferredNames,
        number_of_genes
      )
  }

  return(combined_df)
}
