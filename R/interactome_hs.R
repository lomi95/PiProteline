#' Human Interactome
#'
#' A data frame representing the human interactome, containing protein names and various scores.
#' Each row corresponds to a protein, and the columns include protein identifiers and associated scores
#' that represent various attributes or metrics relevant to protein-protein interactions.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{protein1}{Character vector of protein Ensembl names.}
#'   \item{protein2}{Character vector of protein Ensembl names.}
#'   \item{p1.mapped}{Character vector of protein names mapped with gene names.}
#'   \item{p2.mapped}{Character vector of protein names mapped with gene names.}
#'   \item{neighborhood}{Numeric vector of scores indicating whether the proteins are located near each other in the genome, suggesting potential functional relationships.}
#'   \item{fusion}{Numeric vector of scores based on evidence of gene fusion events, suggesting that two proteins may function together.}
#'   \item{cooccurence}{Numeric vector of scores reflecting the co-occurrence of genes across species, suggesting a functional association.}
#'   \item{coexpression}{Numeric vector of scores indicating the correlation of gene expression patterns, suggesting that the proteins may be involved in related processes.}
#'   \item{experimental}{Numeric vector of scores derived from experimental evidence of protein-protein interactions, such as binding or complex formation.}
#'   \item{database}{Numeric vector of scores based on interactions reported in curated databases, providing additional confidence based on prior knowledge.}
#'   \item{textmining}{Numeric vector of scores derived from automated text mining of scientific literature, indicating potential interactions mentioned in publications.}
#'   \item{combined_score}{Numeric vector representing the overall confidence score for the interaction, combining evidence from all the other sources. Each individual score is integrated to provide a single probability that the interaction is real.}
#' }
#'
#' @source  https://stringdb-downloads.org/download/protein.links.detailed.v12.0/9606.protein.links.detailed.v12.0.txt.gz
#'
#' @examples
#' # Load the interactome data frame
#' data(interactome_hs)
#'
#' # View the first few rows of the interactome
#' str(interactome_hs)
#'
"interactome_hs"
