#' remove_duplicates
#'
#' @param dataset The dataset in which we want to remove duplicates
#' @param genes name of the column containing the Gene names, or vector containing
#'     as elements as the number of dataset rows
#' @param FUN the function applied to duplicated values, default max
#' @param NAasZero if TRUE all NAs will be converted in zeros, default T
#'      if both NAasZero and ZeroasNA are TRUE, it will give an error
#' @param ZeroasNA if TRUE all zeros will be converted in NAs, default F
#'
#' @return A data.frame with no more duplicated Genes, with a new gene column called GeneName
#' @export
#'
remove_duplicates <- function(dataset, genes, FUN = max, NAasZero = T, ZeroasNA = F){
  if (NAasZero & ZeroasNA){
    stop("Both 'NAasZero' and 'ZeroasNA' are TRUE")
  }

  L <- length(genes)
  if (is.character(genes)){
    if (L == 1){
      genes_id <- dataset[,genes]

    } else if (L == nrow(dataset)){
      genes_id <- genes

    } else {
      stop("Length of 'genes' differs from 'ncol(dataset)'")
    }

    dataset.1 <- aggregate(dataset[], list(GeneName = genes_id), FUN = function(x){
      if (ZeroasNA){
        x[as.numeric(x) == 0] <- NA
      } else if (NAasZero){
        x[is.na(x)] <- 0
      }
      return(suppressWarnings(FUN(as.numeric(x))))
    })
    colSumsNA <- apply(dataset.1, 2, function(x){sum(is.na(x))})
    if (sum(colSumsNA)){

      cNA <- colnames(dataset.1)[colSumsNA == nrow(dataset.1)]

      if (length(cNA) == 1){
        if (length(genes)==1){
          if (cNA == genes){
            message("'",genes,"' column is replaced with 'GeneName'")
          }
        }

      } else {

        message(paste(cNA, collapse = ", "),
                " were dropped as not numeric columns")
      }

      dataset.1[,colSumsNA == nrow(dataset.1)] <- NULL
      rownames(dataset.1) <- dataset.1$GeneName

    }

    return(dataset.1)

  } else {
    stop("'genes' is not either a name of a column or a vector of character")
  }

}
