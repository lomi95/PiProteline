#' Plot a circular enrichment tree from single_profile_enrichment results
#'
#' This function generates a circular dendrogram plot of enriched functional categories,
#' using hierarchical clustering based on gene set overlaps and bar plots indicating
#' relative enrichment per group. \code{\link{treeplot2}} is designed to better visualize the
#' pairwise results, \code{\link{treeplot}} is designed to visualize the results of
#' multiple comparisons.
#'
#' @param category_spe A data frame, typically the output of \code{\link{single_profile_enrichment}}.
#'   It must contain columns: \code{term}, \code{category}, \code{description}, \code{pvalue_manova},
#'   one or more columns with "Mean" in the name, and one or more columns with "enriched_in" listing gene identifiers.
#' @param title_plot A character string used as the plot title and as the filename (SVG) of the saved figure.
#' @param first_n Integer. Number of top terms (ranked by \code{pvalue_manova}) to include. Default is 30.
#' @param min_size_enrichments Integer. Minimum number of enriched genes for a term to be included. Default is 0.
#' @param max_size_enrichments Integer. Maximum number of enriched genes for a term to be included. Default is Inf.
#' @param save_plot Boolean. If TRUE, the function will save enrichment plots for each
#' category. Default is TRUE.
#'
#' @return A `ggtree` object with a circular layout and fruit bar plots for each enriched term.
#' An `.svg` file is also saved in the working directory using \code{title_plot} as filename.

#' @import ggtree
#' @import ggplot2
#' @importFrom ape as.phylo.hclust
#' @importFrom ggtreeExtra geom_fruit
#' @importFrom stats dist hclust as.dendrogram as.hclust
#' @importFrom purrr set_names
#' @importFrom dplyr arrange select mutate filter relocate
#' @importFrom utils head
#'
#' @seealso \code{\link{single_profile_enrichment}} for generating the enrichment results.

#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming df is a properly formatted enrichment result
#' spe_result <- single_profile_enrichment(dataset, c("group1","group2","group3"), tax_ID = 9606)
#' treeplot2(spe_result$Process, "Process", first_n = 20)
#' }
#'

treeplot2 <- function(category_spe, title_plot,
                     first_n = 30,
                     min_size_enrichments = 0,
                     max_size_enrichments = Inf,
                     save_plot = T){

  single_spe_filter <- category_spe %>%
    dplyr::arrange(pvalue_manova) %>%
    dplyr::select(term, category, description,
                  dplyr::contains("Mean"), dplyr::contains("enriched_in"))

  genes_enriched <- single_spe_filter[,grep("enriched_in", colnames(single_spe_filter), value = TRUE)] %>%
    apply(1, function(x) sort(unique(unlist(x)))) %>%
    purrr::set_names(single_spe_filter$description)

  mean_normalized <- single_spe_filter[,grep("Mean", colnames(single_spe_filter), value = TRUE)] %>%
    apply(1, function(x) x / sum(x)) %>%
    t

  length_genes_enriched <- sapply(genes_enriched, length)

  proteins <- sort(unique(unlist(genes_enriched)))
  pathways <- single_spe_filter$description
  tab_pathways_protein <- matrix(ncol=length(proteins),nrow = length(pathways),data = 0,
                                 dimnames = list(pathways,proteins))
  for (i in names(genes_enriched)){
    tab_pathways_protein[i,match(genes_enriched[[i]],proteins)] <- 1
  }

  single_spe_filter2 <- data.frame(
    single_spe_filter %>%
      dplyr::select(term, category, description),
    mean_normalized
  ) %>%
    dplyr::mutate(genes_enriched = genes_enriched,
                  length_genes_enriched = length_genes_enriched) %>%
    dplyr::filter(length_genes_enriched >= min_size_enrichments,
                  length_genes_enriched <= max_size_enrichments) %>%
    head(first_n) %>%
    dplyr::relocate(genes_enriched, length_genes_enriched, .after = description)

  # Compute Jaccard similarity matrix
  tpp <- tab_pathways_protein[single_spe_filter2$description,]
  tpp <- tpp[,colSums(tpp) > 0] # Remove empty pathways

  av_columns <- colnames(single_spe_filter2)[-(1:5)]
  single_spe_filter2$group <- apply(single_spe_filter2[,-(1:5)],1,function(x) av_columns[which.max(x)])
  single_spe_filter2$group <- as.factor(single_spe_filter2$group)
  single_spe_filter2$max_val   <- apply(single_spe_filter2[,-c(1:5,ncol(single_spe_filter2))],1,function(x) max(x))

  dist_matrix <- dist(tpp, method = "binary")
  hclust_object <- hclust(dist_matrix, method = "ward.D2")
  dendrogram_object <- as.dendrogram(hclust_object)
  newick <-  ape::as.phylo.hclust(stats::as.hclust(dendrogram_object))

  wrap_after_space <- function(text, n = 20) {
    sapply(text, function(x) {
      # Se il testo è più corto di 1.5 * n, non inserire \n
      if (nchar(x) <= 1.4 * n) return(x)

      # Cerca il primo spazio dopo il carattere n
      rest <- substr(x, n + 1, nchar(x))
      space_pos <- regexpr(" ", rest)[1]

      # Se non ci sono spazi, restituisci il testo originale
      if (space_pos == -1) return(x)

      # Calcola il punto di taglio e inserisci \n
      cut_point <- n + space_pos
      paste0(substr(x, 1, cut_point - 1), "\n", substr(x, cut_point + 1, nchar(x)))
    }, USE.NAMES = FALSE)
  }


  single_spe_filter2$description_wrapped <- wrap_after_space(single_spe_filter2$description, n = 20)
  newick$tip.label <- single_spe_filter2$description_wrapped

  treePlot <- ggtree(newick, branch.length = "none", layout = "circular", open.angle = 15)  +
    ggtitle(title_plot) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_fruit(
      data = single_spe_filter2,
      geom = geom_col,
      mapping = aes(y = description_wrapped, x = max_val, fill = group),
      grid.params=list(),
      axis.params=list(axis="x", text.size = 2, nbreak = 1)
    ) +
    xlim(0, 20) +
    geom_tiplab(size = 3.5, offset = 4)

  if (save_plot){
    ggsave(paste0(title_plot, ".svg"), plot = treePlot, device = "svg",
           width = 20, height = 20)
  }


  return(treePlot)
}
