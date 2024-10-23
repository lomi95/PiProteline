#' Build Interactome from StringDB Data
#'
#' This function builds a protein-protein interaction (PPI) network, or interactome,
#' from a StringDB interactome file. Optionally, it can filter the interactome by score
#' thresholds and annotate protein interactions using StringDB API.
#'
#' @param directory_interactome A string specifying the directory of the StringDB interactome file.
#'        This parameter is ignored if `interactome` is provided.
#' @param scores_threshold A numeric vector of threshold scores to filter the interactome by.
#'        If `NULL`, no filtering is applied.
#' @param tax_ID An integer specifying the taxonomy ID of the species for annotation.
#' @param interactome A data frame of interactome data. Default is `NULL`, in which case
#'        the function will read the file specified by `directory_interactome`.
#' @param AnnNbyN An integer vector for splitting the protein list during annotation to
#'        avoid API timeout errors. Lower values may prevent timeouts. Default is `1999`.
#'
#' @details
#' The function first filters the input interactome based on the provided score thresholds.
#' It then retrieves the protein-protein interactions and annotations from the StringDB
#' database using the `protein_mapping` function.
#'
#' @return A data frame representeing the interactome containing different scores for each couple of proteins.
#'
#' @examples
#' \dontrun{
#'   destfile <- "C:/Users/WKS/Downloads/STRINGDB_intractome.HS.txt.gx"
#'   download.file("https://stringdb-downloads.org/download/protein.links.full.v12.0/
#'                  9606.protein.links.full.v12.0.txt.gz",
#'                  destfile = destfile)
#'   interactome_hs <- build_interactome(directory_interactome = destfile,tax_ID = 9606)
#' }
#'
#' @seealso \code{\link{protein_mapping}}, \code{\link{filter_interactome}}
#'
#'
#' @importFrom utils read.delim
#' @export

build_interactome <- function(directory_interactome = NULL, tax_ID,
                              interactome = NULL,
                              scores_threshold = NULL,
                              AnnNbyN = 1999){

  if (is.null(interactome)){
    interactome <- read.delim(directory_interactome, sep = " ")
  }

  interactome.filtered <- filter_interactome(interactome,
                                             scores_threshold = scores_threshold)


  all.interacting.proteins <- unique(c(interactome.filtered$protein1,
                                       interactome.filtered$protein2))

  mapped_id <- protein_mapping(all.interacting.proteins, tax_ID, AnnNbyN)
  p1.mapped <- mapped_id$preferredName[match(interactome.filtered$protein1,
                                                     mapped_id$queryItem)]
  p2.mapped <- mapped_id$preferredName[match(interactome.filtered$protein2,
                                                     mapped_id$queryItem)]

  interactome.final <- cbind.data.frame(interactome[,1:2],
                                        p1.mapped,p2.mapped,
                                        interactome[,3:10])

  return(interactome.final)
}

