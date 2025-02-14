#' Flatten Correlation Matrix
#'
#' This function takes correlation matrices and flattens them into a data frame format,
#' making it easier to work with the correlation results.
#'
#' @param cormat A matrix of correlation values.
#' @param pmat A matrix of correlation p-values.
#' @param nmat A matrix of observation pairs used for the correlation calculation.
#'
#' @return A data frame containing the flattened correlation matrices, which includes:
#' \item{row}{The row names of the correlation matrix.}
#' \item{column}{The column names of the correlation matrix.}
#' \item{cor}{The correlation values.}
#' \item{p}{The p-values associated with the correlations.}
#' \item{n}{The number of observations for each correlation.}
#' @export
#'
#' @examples
#'
#' library(Hmisc)
#' # Create a sample dataset
#' data <- matrix(rnorm(100), ncol = 5)
#' colnames(data) <- c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5")
#'
#' # Calculate the correlation
#' corr_result <- rcorr(data)
#'
#' # Flatten the correlation matrix
#' flattened_corr <- flattenCorrMatrix(corr_result$r, corr_result$P, corr_result$n)
#' print(flattened_corr)
#'
flattenCorrMatrix <- function(cormat, pmat, nmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = cormat[ut],
    p = pmat[ut],
    n = nmat[ut]
  ) %>%
    filter(
      row != "",
      column != ""
    )
}
