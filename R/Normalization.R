
#' Normalization
#'
#' @param dataset The dataset that we want to normalize Protein x Samples
#' @param normType type of nornalization, choose from:
#'     ln - natural logarithm transformation
#'     Znorm - zeta normalization
#'     MinMax - Min-Max scaling
#'     Robust - Robust scaling
#'     UnitVector - Unit vector scaling
#'     TotSigNorm - total signal normalization
#'     MaxSigNorm - maximum signal normalization
#'     RowSigmaNorm - row sigma normalization
#'
#'
#' @importFrom stats median
#' @importFrom stats IQR
#'
#' @return data normalized
#' @export
#'
#'
Normalization <- function(dataset,normType){
  ### ln (natural logarithm), Z-score, Total Signal, Maximum Signal, Row Sigma

  # We take away the non numeric columns
  asnum.NA <- apply(dataset,2, function(x){
    suppressWarnings(sum(is.na(as.numeric(x))) > length(x)*3/4)
  })
  old.col <- colnames(dataset)[asnum.NA]
  data.char <- data.frame(dataset[,asnum.NA])
  colnames(data.char) <- old.col
  dataset <- dataset[,!asnum.NA]

  data.norm <- switch (normType,

                       # The ln transformation is obtained by taking the natural logarithm of every value
                       # and aims at increasing the signal of the PIDs with low spectral counts with
                       # respect to the more abundant PIDs.
                       ln = log(dataset+1),

                       # The Z normalization is achieved by subtracting from each original value
                       # the mean of all values of the corresponding PID
                       # and dividing the result by the standard deviation of
                       # all values from the same PID; the mean then becomes 0 and the standard deviation 1.
                       Znorm = apply(dataset,2,function(x){
                         return((x-mean(x))/stats::sd(x))
                       }),


                       # Normalizzazione Min-Max
                       # Trasnform data in order to have value between 0 and 1.
                       # Useful when you want same range datasets
                       MinMax = apply(dataset,2,function(x) {
                         return ((x - min(x)) / (max(x) - min(x)))
                       }),

                       # Robust Scaling
                       # Uses median and quantiles to scale data,
                       # usefule whan data have outlier.
                       Robust = apply(dataset,2,function(x) {
                         return ((x - median(x)) / IQR(x))
                       }),

                       # Unit Vector Scaling
                       # Scala data in order to have their norm equal to 1.
                       UnitVector <- apply(dataset,2,function(x) {
                         return (x / sqrt(sum(x^2)))
                       }),


                       # The Total Signal normalization is achieved by dividing each value by
                       # the sum of all values in the respective row.
                       TotSigNorm = apply(dataset,2,function(x){
                         return(x/sum(x))
                       }),

                       # The Maximum Signal normalization
                       # is obtained by dividing each value by the largest value in its row;
                       # an underlying assumption is that, in each MudPIT analysis,
                       # peptide identifications were obtained at or near the capacity of
                       # the tandem MS instrument.
                       MaxSigNorm = apply(dataset,2,function(x){
                         return(x/max(x))
                       }),


                       # The Row Sigma normalization is achieved
                       # by calculating the mean and standard deviation of all values in a row and
                       # then dividing each value by the mean plus three standard deviations.
                       # The latter is introduced in this work as a variation of the Maximum Signal
                       # normalization that better handles assays that obtained an exceedingly high
                       # maximum value for a protein; further advantages are addressed in the Results
                       # and discussion section.
                       #
                       RowSigmaNorm = apply(dataset,2,function(x){
                         return(x/(mean(x)+3*stats::sd(x)))
                       })
  )

  return(data.frame(data.char,data.norm))
}
