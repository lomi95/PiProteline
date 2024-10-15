#' Flatten Correlation Output for PPI Analysis
#'
#' Converts the output of the \link[Hmisc]{rcorr} function into a flattened data frame format.
#'
#' @param Corr A list containing the output of the \link[Hmisc]{rcorr} function, including matrices
#'     for correlation coefficients (`r`), p-values (`P`), and number of observations (`n`).
#'
#' @details The function extracts the correlation coefficients, p-values, and number of observations
#'     from the input `rcorr` object and flattens them into a single data frame. Missing values
#'     in the correlation matrix are replaced with zero, while missing values in the p-value matrix
#'     are replaced with one.
#'
#' @importFrom Hmisc rcorr
#'
#' @return A data frame containing the flattened correlation data, including the correlation
#'     features, correlation coefficients, p-values, and number of observations.
#' @examples
#' # Example usage with Hmisc::rcorr
#' library(Hmisc)
#' data <- matrix(rnorm(100), ncol = 5)
#' colnames(data) <- c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5")
#' corr_result <- rcorr(data)
#'
#' # Flatten correlation output
#' flattened_corr <- flatten2CorrPPI(corr_result)
#' print(flattened_corr)
#'
#' @export
flatten2CorrPPI <- function(Corr){
  flattCorr <- flattenCorrMatrix(Corr$r, Corr$P, Corr$n)
  flattCorr$cor_features <- paste(flattCorr$row, "and", flattCorr$column)
  flattCorr$cor[is.na(flattCorr$cor)] <- 0
  flattCorr$p[is.na(flattCorr$p)] <- 1

  return(flattCorr)
}
