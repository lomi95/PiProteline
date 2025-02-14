#' Volcano plot for Differential Expression Analysis
#'
#' This function generates a volcano plot to visualize differential expression analysis results, highlighting genes that are significantly up-regulated or down-regulated based on log-fold change and p-value thresholds.
#'
#' @param t_dataset A numeric matrix or data frame where rows represent features (e.g., genes)
#' and columns represent samples. The last row should contain p-values.
#' @param fc_bounds A numeric vector of length 2 specifying the fold change boundaries. Genes with log2 fold changes outside this range are highlighted in red (up-regulated) or blue (down-regulated).
#' @param significance A numeric value indicating the p-value threshold. Genes with p-values below this threshold (after applying -log10 transformation) are considered significant.
#' @param group_1 A character string specifying the name of the first (upregulated positive) group. Default is "Group1".
#' @param group_2 A character string specifying the name of the second group. Default is "Group2".
#' @param ignore_case Logical; if TRUE (default), group names are matched ignoring case.
#'
#' @importFrom ggplot2 aes geom_point scale_color_manual geom_vline geom_hline theme_minimal theme geom_label labs unit
#' @importFrom ggrepel geom_text_repel
#'
#' @return A `ggplot` object representing the volcano plot
#'
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
#'
#' @export
volcano_plot <- function(t_dataset, significance = 0.05, fc_bounds = c(-0,0), ignore_case = T,
                         group_1 = "Group1", group_2 = "Group2") {

  g1g2 <- colnames(t_dataset)
  g1 <- colMeans(t_dataset[grep(group_1,rownames(t_dataset),ignore.case = ignore_case),, drop = F])
  g2 <- colMeans(t_dataset[grep(group_2,rownames(t_dataset),ignore.case = ignore_case),, drop = F])
  df <- data.frame(log2 = log2(g1/g2)[g1g2],
                   pv   = as.vector(t(-log10(t_dataset[nrow(t_dataset),]))),
                   gene = g1g2)
  df$gene[df$log2 >= min(fc_bounds) & df$log2 <= max(fc_bounds)] <- ""

  significance.log <- -log10(significance)
  log2 <- df$log2
  pv <- df$pv
  df$color <- "Non-significant"
  df$color[log2 >= max(fc_bounds) & pv >= significance.log] <- "up-regulated"
  df$color[log2 <= min(fc_bounds) & pv >= significance.log] <- "down-regulated"
  df$color <- as.factor(df$color)

  df$gene[df$color == "Non-significant"] <- ""

  num_up <- sum(df$color == "up-regulated")
  num_down <- sum(df$color == "down-regulated")

  Vplot <- ggplot(df, aes(x = log2, y = pv)) +
    geom_point(aes(color = color)) +
    scale_color_manual(values = c("up-regulated" = "red",
                                  "down-regulated" = "blue",
                                  "Non-significant" = "grey")) +
    geom_vline(xintercept = min(fc_bounds), linetype = "dashed", color = "black") +
    geom_vline(xintercept = max(fc_bounds), linetype = "dashed", color = "black") +
    geom_hline(yintercept = significance.log, linetype = "dashed", color = "black") +
    geom_text_repel(aes(label = gene),
                    hjust = -0.2, vjust = -0.2,
                    box.padding = 0.35, point.padding = 0.3,
                    segment.color = 'grey50',
                    max.overlaps = 5) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste0("volcano plot - UP (red) ", group_1, " vs DOWN (blue) ", group_2),
         x = "log2(FC)",
         y = "-log10(p.adjusted)") # +
    # geom_label(
    #   x = min(log2[log2 != -Inf]) + (max(log2[log2 != Inf]) - min(log2[log2 != -Inf])) / 18, y = max(pv),
    #   label = paste0("Down-regulated: ", num_down),
    #   label.padding = unit(0.55, "lines"),
    #   label.size = 0.35,
    #   color = "blue",
    #   fill = NA) +
    # geom_label(
    #   x = max(log2[log2 != Inf]) - (max(log2[log2 != Inf]) - min(log2[log2 != -Inf])) / 18, y = max(pv),
    #   label = paste0("Up-regulated: ", num_up),
    #   label.padding = unit(0.55, "lines"),
    #   label.size = 0.35,
    #   color = "red",
    #   fill = NA)

  return(Vplot)
}
