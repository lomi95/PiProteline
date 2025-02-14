#' emPAI (Exponentially Modified Protein Abundance Index)
#'
#' This function calculates the emPAI (Exponentially Modified Protein Abundance Index) for a set of peptides, either provided as a list of groups or as a dataset.
#'
#' @param list_groups_peptides A list where each element represents a group of peptides (e.g., numeric vectors or matrices of peptide data), or a dataset where rows are samples and columns are proteins.
#' @param ideal_peptides A numeric vector representing the ideal number of peptides for each protein.
#'
#' @return The emPAI matrix (if input is a list) or vector (if input is a dataset) for each protein.
#'
#' @details The emPAI index provides an estimate of the relative abundance of proteins in a sample based on the number of observable and observed peptides. It is calculated as:
#' \deqn{emPAI = \frac{10^{(Obs/Pn_{ideal}) - 1}}{\sum 10^{(Obs/Pn_{ideal}) - 1}}}
#' where \code{Obs} is the observed number of peptides, and \code{ideal_peptides} is the ideal number of peptides for each protein.
#'
#' @references Ishihama, Y., Oda, Y., Tabata, T., Sato, T., Nagasu, T., Rappsilber, J., & Mann, M. (2005). Exponentially modified protein abundance index (emPAI) for estimation of absolute protein amount in proteomics by the number of sequenced peptides per protein. *Molecular & Cellular Proteomics*, 4(9), 1265-1272. \doi{10.1074/mcp.M500061-MCP200}.
#' Available at: \href{https://pubmed.ncbi.nlm.nih.gov/15958392/}{PubMed - Exponentially modified protein abundance index}.
#'
#' @examples
#' peptides_group1 <- matrix(c(5, 10, 15), nrow = 1)
#' peptides_group2 <- matrix(c(8, 12, 20), nrow = 1)
#' ideal_peptides <- c(10, 10, 10)
#' list_groups_peptides <- list(Group1 = peptides_group1, Group2 = peptides_group2)
#' emPAI_function(list_groups_peptides, ideal_peptides)
#'
#' # Using a dataset (Sample x Protein)
#' dataset <- matrix(c(5, 10, 15, 8, 12, 20), nrow = 2)
#' emPAI_function(dataset, ideal_peptides)
#'
#' @export
emPAI_function <- function(list_groups_peptides, ideal_peptides){

  # Pept osservabili = siti tripsina = ARGININA/LISINA & 5/6 AA di lunghezza peptide

  if (is.list(list_groups_peptides)){
    Empai <- sapply(list_groups_peptides, function(x){
      (10^(colMeans(x) / ideal_peptides - 1)) / sum(10^(colMeans(x) / ideal_peptides - 1))
    })
  } else {
    Empai <- (10^(colMeans(list_groups_peptides) / ideal_peptides - 1)) / sum(10^(colMeans(list_groups_peptides) / ideal_peptides - 1))
  }

  return(Empai)
}
