#' Transpose a Dataset
#'
#' This function transposes a given `data.frame`, optionally setting the column names of the transposed `data.frame` using a specified column from the original dataset or using the `rownames`.
#'
#' @param dataset A data frame to transpose.
#' @param gene_column The name or index of the column to use for setting the `colnames` of the transposed data. If set to 0, the `rownames` will be used as `colnames` Default is 1.
#'
#' @return A transposed data frame with updated column names.
#' @examples
#' # Example with a simple dataset
#' data <- data.frame(
#'   Gene = c("Gene1", "Gene2", "Gene3"),
#'   Sample1 = c(1, 4, 7),
#'   Sample2 = c(2, 5, 8),
#'   Sample3 = c(3, 6, 9)
#' )
#'
#' # Transpose using the "Gene" column as new column names
#' transpose(data, gene_column = "Gene")
#'
#' # Transpose using row names as new column names
#' rownames(data) <- data$Gene
#' transpose(data, gene_column = 0)
#'
#' @export
transpose <- function(dataset, gene_column = 1) {
  if (gene_column == 0) {
    old.rwnms <- rownames(dataset)
    df.t <- data.frame(t(dataset))
    colnames(df.t) <- old.rwnms
  } else {
    df.genes <- dataset[, gene_column]
    dataset[, gene_column] <- NULL
    df.t <- data.frame(t(dataset))
    colnames(df.t) <- df.genes
  }

  return(df.t)
}
