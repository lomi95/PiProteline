#' convert_ID
#'
#' Converts gene identifiers in a dataset to the preferred gene name using the specified taxonomy ID.
#'
#' @param dataset A matrix or data frame containing the gene information.
#' @param gene_column An integer indicating the position of the column that contains the gene identifiers.
#' @param tax_ID An integer representing the taxonomy ID of the species.
#'
#' @return The original dataset with the gene identifiers converted to the preferred gene names.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' dataset <- data.frame(GeneName = c("BRCA1", "TP53", "EGFR"),
#'                       Value = c(5.6, 3.8, 7.2))
#'
#' # Convert gene identifiers for human (tax_ID = 9606)
#' converted_dataset <- convert_ID(dataset, gene_column = 1, tax_ID = 9606)
#' }
#' @export
convert_ID <- function(dataset, gene_column, tax_ID) {
  # Map the gene IDs using rbioapi
  mapid <- rbioapi::rba_string_map_ids(dataset[, gene_column], tax_ID)

  # Display a message if some gene names were not mapped
  unmapped_genes <- dataset[, gene_column][!dataset[, gene_column] %in% mapid$queryItem]
  if (length(unmapped_genes) > 0) {
    message(paste(paste(unmapped_genes, collapse = ", "), "was/were not mapped to Gene Name"))
  }

  # Update the gene names in the dataset
  dataset[dataset[, gene_column] %in% mapid$queryItem, gene_column] <- mapid$preferredName

  return(dataset)
}
