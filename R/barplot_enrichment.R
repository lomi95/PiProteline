#' Create and save a horizontal barplot of enrichment results
#'
#' This function generates a horizontal barplot from functional enrichment results
#' (e.g., GO terms or pathways), showing the proportion of enriched genes (`ratio_in_set`)
#' per term, colored by FDR. Only terms within the specified gene set size range are shown.
#'
#' @param comparisons_enrichment A named list of data frames, one per comparison.
#' @param comparison A string indicating the name of the comparison (e.g., "SN_C_vs_SN_PD").
#' @param subtitle A string to append to the subtitle of the plot (optional).
#' @param first_n Integer. The number of top terms to show (default = 30).
#' @param min_size_enrichments Integer. Minimum number of genes in background to consider (default = 0).
#' @param max_size_enrichments Integer. Maximum number of genes in background to consider (default = Inf).
#' @param save_plot Logical. Whether to save the plot as an SVG file (default = TRUE).
#'
#' @return A ggplot object representing the enrichment barplot.
#' @export
#'
#' @examples
#' \dontrun{
#'   my_enrichment <- enrichment_manova(prot_manova, tax_ID, categories)
#'   barplot_enrichment(my_enrichment, comparison = "GroupA_vs_GroupB")
#' }
barplot_enrichment <- function(comparisons_enrichment,
                               comparison, subtitle = "",
                               first_n = 30,
                               min_size_enrichments = 0,
                               max_size_enrichments = Inf,
                               save_plot = T
                               ){
  comparison_filtered <- comparisons_enrichment %>%
    arrange(desc(fdr)) %>%
    head(first_n) %>%
    filter(number_of_genes_in_background >= min_size_enrichments,
           number_of_genes_in_background <= max_size_enrichments) %>%
    mutate(
      ratio_in_set = number_of_genes / number_of_genes_in_background,
      description = factor(description, levels = unique(description))
    )

  enr_plot <- ggplot(data = comparison_filtered,
         aes(x = description,
             y = ratio_in_set,
             fill = fdr)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(x = NULL, y = "Ratio in set", fill = "FDR") +
    ggtitle(comparison, subtitle = subtitle)

  if (save_plot) {
    ggsave(filename = paste0(comparison, "_", subtitle, ".svg"),
           plot = enr_plot, width = 15, height = nrow(comparison_filtered)/3, device = "svg")
  }

  return(enr_plot)
}




