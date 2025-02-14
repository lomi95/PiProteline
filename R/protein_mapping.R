#' Map Protein IDs Using StringDB
#'
#' This function maps a vector of protein IDs using the StringDB API.
#' It splits the input into smaller chunks to retrieve annotations and mapped IDs,
#' which are then combined into a final list of data frames.
#'
#' @param proteins_ids A character vector of protein IDs that need to be annotated.
#' @param splitting_API An integer specifying the number of proteins to process per batch in the API call.
#' @param tax_ID An integer specifying the taxonomy ID of the species to use for the annotation.
#'
#' @details
#' The function interacts with the `rba_string_annotations` and `rba_string_map_ids` functions
#' from the `rbioapi` package to fetch annotations and map protein IDs. The input vector is split
#' into smaller chunks to avoid overloading the API. Annotations with more than one gene are kept
#' in the final output.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{mapped_id}{A data frame with the mapped protein IDs.}
#'   \item{annotation.df}{A data frame containing the annotations for the proteins that map to more than one gene.}
#' }
#'
#' @examples
#' \dontrun{
#'   proteins <- c("P12345", "Q67890", "A11111")
#'   mapped_id <- protein_mapping(proteins, tax_ID = 9606, splitting_API = 100)
#'   mapped_id  # Mapped IDs
#' }
#'
#' @seealso \code{\link[rbioapi]{rba_string_map_ids}}
#'
#' @importFrom rbioapi rba_string_map_ids
#' @export
protein_mapping <- function(proteins_ids, tax_ID, splitting_API){

  vect.ann <- seq(1,length(proteins_ids),by = splitting_API)

  all.mapping <- list()
  for (i in 1:(length(vect.ann)-1)){
    all.mapping[[i]] <- rba_string_map_ids(
      proteins_ids[vect.ann[i]:(vect.ann[i+1]-1)],tax_ID,verbose = F)
  }
  all.mapping[[i+1]] <- rba_string_map_ids(
    proteins_ids[vect.ann[i+1]:length(proteins_ids)],tax_ID, verbose = F)


  map_id.merged <- Reduce(rbind,all.mapping)


  return(map_id.merged)
}
