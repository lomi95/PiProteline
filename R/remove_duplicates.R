#' Remove Duplicate Rows Based on Gene Names
#'
#' This function removes duplicate rows from a dataset based on a specified column containing gene names. It applies a specified function to resolve duplicates and allows handling of missing values (NA) and zeros.
#'
#' @param dataset A data frame from which duplicate rows should be removed.
#' @param genes An integer specifying the column index, a character specifying the column name for gene names, or a character vector containing gene names for each row. The length of the vector must be equal to the number of rows in the dataset.
#' @param FUN A function to apply to duplicate values for resolving duplicates. Default is `max`.
#' @param NAasZero A logical value indicating if NAs should be treated as zeros. Default is `TRUE`. If both `NAasZero` and `ZeroasNA` are `TRUE`, an error will be raised.
#' @param ZeroasNA A logical value indicating if zeros should be treated as NAs. Default is `FALSE`.
#'
#' @return A data frame with duplicate rows removed, based on the specified gene column. The resulting data frame includes a new column named `GeneName` for the gene identifiers.
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
#'   Value1 = c(5, 2, 7, 3),
#'   Value2 = c(NA, 4, 1, 8)
#' )
#'
#' # Remove duplicates based on the "Gene" column, using the default max function
#' remove_duplicates(data, genes = "Gene")
#'
#' # Handle NAs as zeros
#' remove_duplicates(data, genes = "Gene", NAasZero = TRUE)
#'
#' @export
remove_duplicates <- function(dataset, genes, FUN = max, NAasZero = TRUE, ZeroasNA = FALSE) {
  if (NAasZero & ZeroasNA) {
    stop("Both 'NAasZero' and 'ZeroasNA' are TRUE")
  }

  L <- length(genes)

  if (is.numeric(genes)) {
    if (L == 1) {
      genes_id <- dataset[, genes]
    } else {
      stop("'genes', if numeric, should be of length 1")
    }
  } else if (is.character(genes)) {
    if (L == 1) {
      genes_id <- dataset[, genes]
    } else if (L == nrow(dataset)) {
      genes_id <- genes
    } else {
      stop("Length of 'genes' differs from number of rows of the dataset")
    }
  } else {
    stop("'genes' is not either an integer, a name of a column or a vector of character")
  }

  dataset.1 <- aggregate(dataset[], list(GeneName = genes_id), FUN = function(x) {
    if (ZeroasNA) {
      x[as.numeric(x) == 0] <- NA
    } else if (NAasZero) {
      x[is.na(x)] <- 0
    }
    return(suppressWarnings(FUN(as.numeric(x))))
  })

  colSumsNA <- apply(dataset.1, 2, function(x) { sum(is.na(x)) })
  if (sum(colSumsNA)) {
    cNA <- colnames(dataset.1)[colSumsNA == nrow(dataset.1)]

    if (length(cNA) == 1 && length(genes) == 1 && cNA == genes) {
      message("'", genes, "' column is replaced with 'GeneName'")
    } else {
      message(paste(cNA, collapse = ", "), " were dropped as non-numeric columns")
    }

    dataset.1[, colSumsNA == nrow(dataset.1)] <- NULL
    rownames(dataset.1) <- dataset.1$GeneName
  }

  return(dataset.1)
}
