#' Summary of LDA Analysis Results
#'
#' This function generates a summary of linear discriminant analysis (LDA) results for pairwise comparisons. It extracts up-regulated and down-regulated genes from the LDA results and constructs a gene matrix indicating the presence of each gene across different comparisons.
#'
#' @param LDA_pairw.results A list containing LDA results for pairwise comparisons, where each comparison includes a list of volcano plots with up-regulated and down-regulated genes.
#'
#' @return A list containing:
#' \describe{
#'   \item{prot_LDA}{A list of up-regulated and down-regulated genes for each comparison.}
#'   \item{gene_matrix}{A data frame where rows represent unique genes and columns indicate the presence of each gene in up-regulated or down-regulated lists for each comparison, with a final column for gene names.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' LDA_pairw.results <- list(
#'   Comparison1 = list(
#'     VolcanoPlots = list(
#'       list(list(Genes = list(UP = c("Gene1", "Gene2"), DOWN = c("Gene3"))))
#'     )
#'   ),
#'   Comparison2 = list(
#'     VolcanoPlots = list(
#'       list(list(Genes = list(UP = c("Gene3", "Gene4"), DOWN = c("Gene1"))))
#'     )
#'   )
#' )
#' summary_LDA(LDA_pairw.results)
#' }
summary_LDA <- function(LDA_pairw.results) {
  prot_LDA <- list()

  # Estrarre geni up-regulated e down-regulated per ciascun confronto
  for (comparison in names(LDA_pairw.results)) {
    up_genes <- LDA_pairw.results[[comparison]]$VolcanoPlots[[1]][[1]]$Genes$UP
    down_genes <- LDA_pairw.results[[comparison]]$VolcanoPlots[[1]][[1]]$Genes$DOWN

    prot_LDA[[paste0(comparison, "_UP")]] <- up_genes
    prot_LDA[[paste0(comparison, "_DOWN")]] <- down_genes
  }

  # Creare la matrice di geni
  all_protLDA <- unique(unlist(prot_LDA))
  gene_matrix <- sapply(prot_LDA, function(g) all_protLDA %in% g)
  rownames(gene_matrix) <- all_protLDA
  colnames(gene_matrix) <- paste0(colnames(gene_matrix), "_LDA")
  gene_matrix <- as.data.frame(gene_matrix) %>% dplyr::mutate(GeneName = rownames(gene_matrix))

  # Restituire i risultati
  return(list(prot_LDA = prot_LDA, gene_matrix = gene_matrix))
}
