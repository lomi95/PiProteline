#' NSAF (Normalized Spectral Abundance Factor)
#'
#' This function calculates the NSAF index for a list of groups or a dataset of samples by proteins. The NSAF index is used in proteomics to normalize spectral counts for protein length and compare relative protein abundance across samples.
#'
#' @param list.groups A list of data frames or matrices representing different groups for which to compute the NSAF index, or a single data frame/matrix where rows represent samples and columns represent proteins.
#' @param length_proteins A numeric vector representing the lengths of the proteins, ordered according to the columns of the dataset.
#'
#' @return A matrix or vector of NSAF indexes for each protein, depending on whether `list.groups` is a list or a single data frame/matrix.
#' @references Zhang, Y., Wen, Z., Washburn, M.P., & Florens, L. (2010). Effect of dynamic exclusion duration on spectral count based quantitative proteomics. *Journal of Proteome Research*, 9(10), 4552–4560. [PMC3599300](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3599300/)
#' @examples
#' # Example data
#' length_proteins <- c(300, 500, 200)
#' samples <- matrix(
#'   c(10, 20, 30,
#'     40, 50, 60,
#'     70, 80, 90),
#'   nrow = 3, byrow = TRUE
#' )
#'
#' # Calculate NSAF index for a single dataset
#' NSAF(samples, length_proteins)
#'
#' # Example with a list of groups
#' group1 <- matrix(
#'   c(5, 10, 15,
#'     20, 25, 30),
#'   nrow = 2, byrow = TRUE
#' )
#'
#' group2 <- matrix(
#'   c(8, 16, 24,
#'     32, 40, 48),
#'   nrow = 2, byrow = TRUE
#' )
#'
#' NSAF(list(group1, group2), length_proteins)
#'
#' @export
NSAF <- function(list.groups, length_proteins) {
  if (is.list(list.groups)) {
    nsaf_index <- sapply(list.groups, function(x) {
      (colMeans(x) / length_proteins) / sum(colMeans(x) / length_proteins)
    })
  } else {
    nsaf_index <- (colMeans(list.groups) / length_proteins) / sum(colMeans(list.groups) / length_proteins)
  }

  return(nsaf_index)
}
