#' flatten2CorrPPI
#'
#' @param Corr output of Hmisc::rcorr()
#' @return A flatten correlation data.frame, used for 'Corr_PPI.model()' function
#' @export
#'
flatten2CorrPPI <- function(Corr){
  flattCorr <- flattenCorrMatrix(Corr$r,Corr$P,Corr$n)
  flattCorr$cor_features <- paste(flattCorr$row,"and",flattCorr$column)
  flattCorr$cor[is.na(flattCorr$cor)] <- 0
  flattCorr$p[is.na(flattCorr$p)] <- 1

  return(flattCorr)
}
