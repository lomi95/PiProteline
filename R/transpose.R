#' transpose
#'
#' @param dataset The dataset to transpose
#' @param gene_column names or indexes of the columns that contain characters,
#'      if 0 rownames will be used as colnames. Default = 1
#'
#' @return A data.frame transposed
#' @export
#'
transpose <- function(dataset, gene_column = 1){

  if (gene_column == 0){
    old.rwnms <- rownames(dataset)
    df.t <- data.frame(t(dataset))
    colnames(df.t) <- old.rwnms
  } else {
    df.genes <- dataset[,gene_column]

    dataset[,gene_column] <- NULL
    df.t <- data.frame(t(dataset))
    colnames(df.t) <- df.genes
  }

  return(df.t)
}
