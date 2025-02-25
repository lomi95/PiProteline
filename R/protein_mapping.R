#' Map Protein IDs Using StringDB
#'
#' This function maps a vector of protein IDs using the StringDB API.
#' It splits the input into smaller chunks to retrieve mapped IDs.
#'
#' @param proteins_ids A character vector of protein IDs that need to be mapped
#' @param splitting_API An integer specifying the number of proteins to process per batch in the API call. Default = 1999.
#' @param tax_ID An integer specifying the taxonomy ID of the species to use for the mapping
#' @param verbose If TRUE messsages about progress will appear
#' @details
#' The function interacts with the `rba_string_map_ids` functions
#' from the `rbioapi` package to map protein IDs. The input vector is split
#' into smaller chunks to avoid overloading the API.
#'
#' @return A data frame with the mapped protein IDs.
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
protein_mapping <- function(proteins_ids, tax_ID, splitting_API = 1999, verbose = F){

  vect.ann <- seq(1,length(proteins_ids),by = splitting_API)

  #ping
  rba_string_map_ids(c("RPL24","RPL26","RPL29","RPL34","RPS15","RPS24"), 9606, verbose = F)

  all.mapping <- list()
  for (i in 1:(length(vect.ann)-1)){
    if (verbose) message("Mapping chunk ", i, " of ", length(vect.ann))
    all.mapping[[i]] <- rba_string_map_ids(
      proteins_ids[vect.ann[i]:(vect.ann[i+1]-1)],tax_ID,verbose = F)
  }
  if (verbose) message("Mapping chunk ", i+1, " of ", length(vect.ann))
  all.mapping[[i+1]] <- rba_string_map_ids(
    proteins_ids[vect.ann[i+1]:length(proteins_ids)],tax_ID, verbose = F)


  map_id.merged <- Reduce(rbind,all.mapping)


  return(map_id.merged)
}
