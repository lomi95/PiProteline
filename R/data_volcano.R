#' Generate  data for Volcano Plots for Group Comparisons
#'
#' This function generates transposed data.frames volcano plots for pairwise comparisons between groups
#' in a given dataset.
#'
#' @param dataset A data frame where columns represent features (e.g., genes)
#' and rows represent samples. The last row should contain p-values.
#' @param manova_pairw_results Output from `manova_pairwise`
#' @param significance A numeric value specifying the significance threshold.
#' @param gene_column A numeric or character value indicating the index or the name of the gene column
#'
#' @return A list of transposed data.frame where the last row is the p.value adjusted, useful for volcano plot.
#'
#' @seealso [volcano_plot()]
#' @export
#'
#' @importFrom dplyr relocate filter mutate
#' @examples
#' set.seed(1)
#' dataset <- data.frame(GeneName = letters,
#'                       matrix(runif(26*15), nrow = 26))
#' colnames(dataset)[-1] <- paste0(rep(c("Group1", "Group2", "Group3"), each = 5), "_",1:15)
#'
#' mp <- manova_pairwise(dataset,c("Group1", "Group2", "Group3"),gene_column = 1 )
#' dv <- data_volcano(dataset, mp, 0.95)
#'
#' vp <- volcano_plot(dv[[1]], 0.95)
data_volcano <- function(dataset,manova_pairw_results,significance, gene_column = NULL){
  if (!is.null(gene_column)){
    if (is.character(gene_column)){
      gene_column <- which(colnames(dataset) == gene_column)
    }

    colnames(dataset)[gene_column] <- "GeneName"

  }

  dataset <- dataset %>% dplyr::relocate(GeneName, .before = 1)
  dv <- lapply(manova_pairw_results, function(x) {

    x <- x %>% filter(GeneName %in% dataset$GeneName)

   dataset %>%
     dplyr::filter(GeneName %in% x$GeneName) %>%
     dplyr::mutate(p.adj = x$p.adj) %>%
     dplyr::filter(p.adj <= significance) %>%
     PiProteline::transpose(1)
  })
  names(dv) <- names(manova_pairw_results)
  return(dv)
}


