#' convert_ID
#'
#' @param dataset matrix or data.frame
#' @param gene_column integer. the position of the gene column
#' @param tax_ID integer. Taxonomy ID of the specie
#'
#' @return The dataset with genes converted
#' @export
#'
convert_ID <- function(dataset, gene_column, tax_ID){
  mapid <- rbioapi::rba_string_map_ids(dataset[,gene_column],tax_ID)
  message(paste0(dataset$GeneName[!dataset$GeneName %in% mapid$queryItem], collapse =", "),
          "was/were not mapped to Gene Name")
  dataset$GeneName[dataset$GeneName %in% mapid$queryItem] <- mapid$preferredName
  return(dataset)
}
