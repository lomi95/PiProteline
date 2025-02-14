#' Data normalization
#'
#' This function normalizes a dataset (Proteins x Samples) using various normalization techniques. It supports different methods for scaling and transforming the data, including natural logarithm transformation, Z-score normalization, Min-Max scaling, and more.
#'
#' @param dataset A data frame where rows represent proteins and columns represent samples.
#' @param norm_type A character string specifying the type of normalization. Choose from:
#' \describe{
#'   \item{ln}{Natural logarithm transformation}
#'   \item{Znorm}{Z-score normalization}
#'   \item{MinMax}{Min-Max scaling}
#'   \item{Robust}{Robust scaling using median and IQR}
#'   \item{UnitVector}{Unit vector scaling}
#'   \item{TotSigNorm}{Total signal normalization}
#'   \item{MaxSigNorm}{Maximum signal normalization}
#'   \item{RowSigmaNorm}{Row sigma normalization}
#' }
#'
#' @importFrom stats median IQR sd
#'
#' @return A data frame with the normalized data.
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   Sample1 = c(10, 20, 30),
#'   Sample2 = c(5, 15, 25),
#'   Sample3 = c(2, 8, 18)
#' )
#'
#' # Normalize using different methods
#' normalization(data, norm_type = "ln")
#' normalization(data, norm_type = "Znorm")
#' normalization(data, norm_type = "MinMax")
#' normalization(data, norm_type = "Robust")
#' normalization(data, norm_type = "UnitVector")
#' normalization(data, norm_type = "TotSigNorm")
#' normalization(data, norm_type = "MaxSigNorm")
#' normalization(data, norm_type = "RowSigmaNorm")
#'
#' @export
normalization <- function(dataset, norm_type) {
  # Remove non-numeric columns
  dataset[is.na(dataset)] <- 0
  asnum.NA <- apply(dataset, 2, function(x) {
    suppressWarnings(sum(is.na(as.numeric(x))) > length(x) * 3 / 4)
  })
  old.col <- colnames(dataset)[asnum.NA]
  data.char <- data.frame(dataset[, asnum.NA])
  colnames(data.char) <- old.col
  dataset <- dataset[, !asnum.NA]

  # Apply the selected normalization technique
  data.norm <- switch(norm_type,
                      ln = log(dataset + 1),
                      Znorm = apply(dataset, 2, function(x) {
                        return((x - mean(x)) / stats::sd(x))
                      }),
                      MinMax = apply(dataset, 2, function(x) {
                        return((x - min(x)) / (max(x) - min(x)))
                      }),
                      Robust = apply(dataset, 2, function(x) {
                        return((x - median(x)) / IQR(x))
                      }),
                      UnitVector = apply(dataset, 2, function(x) {
                        return(x / sqrt(sum(x^2)))
                      }),
                      TotSigNorm = apply(dataset, 2, function(x) {
                        return(x / sum(x))
                      }),
                      MaxSigNorm = apply(dataset, 2, function(x) {
                        return(x / max(x))
                      }),
                      RowSigmaNorm = apply(dataset, 2, function(x) {
                        return(x / (mean(x) + 3 * stats::sd(x)))
                      })
  )

  return(data.frame(data.char, data.norm))
}
