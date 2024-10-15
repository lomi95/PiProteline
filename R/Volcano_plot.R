#' Volcano Plot for Differential Expression Analysis
#'
#' This function generates a volcano plot to visualize differential expression analysis results, highlighting genes that are significantly up-regulated or down-regulated based on log-fold change and p-value thresholds.
#'
#' @param df A data frame with columns named `log2` (log2 fold change, x-axis), `pv` (-log10 of the p-value, y-axis), and `gene` (gene names or labels).
#' @param boundFC A numeric vector of length 2 specifying the fold change boundaries. Genes with log2 fold changes outside this range are highlighted in red (up-regulated) or blue (down-regulated).
#' @param thr_pvalue A numeric value indicating the p-value threshold. Genes with p-values below this threshold (after applying -log10 transformation) are considered significant.
#' @param Name_1 A character string specifying the name of the first group. Default is "Group1".
#' @param Name_2 A character string specifying the name of the second group. Default is "Group2".
#'
#' @importFrom ggplot2 aes geom_point scale_color_manual geom_vline geom_hline theme_minimal theme geom_label labs unit
#' @importFrom ggrepel geom_text_repel
#'
#' @return A list containing:
#' \describe{
#'   \item{plot}{A `ggplot` object representing the volcano plot.}
#'   \item{Genes}{A list of up-regulated and down-regulated gene names.}
#' }
#'
#' @examples
#' # Example data
#' example_data <- data.frame(
#'   log2 = rnorm(1000),
#'   pv = -log10(runif(1000)),
#'   gene = paste0("Gene", 1:1000)
#' )
#' # Generate volcano plot
#' result <- Volcano_Plot(example_data, boundFC = c(-1, 1), thr_pvalue = 0.05)
#' # Display the plot
#' print(result$plot)
#' # List up-regulated and down-regulated genes
#' result$Genes
#'
#' @export
Volcano_Plot <- function(df, boundFC, thr_pvalue, Name_1 = "Group1", Name_2 = "Group2") {
  thr_pvalue.log <- -log10(thr_pvalue)
  log2 <- df$log2
  pv <- df$pv
  df$color <- "Non-significant"
  df$color[log2 >= max(boundFC) & pv >= thr_pvalue.log] <- "up-regulated"
  df$color[log2 <= min(boundFC) & pv >= thr_pvalue.log] <- "down-regulated"
  df$color <- as.factor(df$color)

  df$gene[df$color == "Non-significant"] <- ""

  num_up <- sum(df$color == "up-regulated")
  num_down <- sum(df$color == "down-regulated")

  Vplot <- ggplot(df, aes(x = log2, y = pv)) +
    geom_point(aes(color = color)) +
    scale_color_manual(values = c("up-regulated" = "red",
                                  "down-regulated" = "blue",
                                  "Non-significant" = "grey")) +
    geom_vline(xintercept = min(boundFC), linetype = "dashed", color = "black") +
    geom_vline(xintercept = max(boundFC), linetype = "dashed", color = "black") +
    geom_hline(yintercept = thr_pvalue.log, linetype = "dashed", color = "black") +
    geom_text_repel(aes(label = gene),
                    hjust = -0.2, vjust = -0.2,
                    box.padding = 0.35, point.padding = 0.3,
                    segment.color = 'grey50',
                    max.overlaps = 5) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste0("Volcano Plot - UP ", Name_1, " vs DOWN ", Name_2),
         x = "log2(FC)",
         y = "-log10(p.adjusted)") +
    geom_label(
      x = min(log2[log2 != -Inf]) + (max(log2[log2 != Inf]) - min(log2[log2 != -Inf])) / 18, y = max(pv),
      label = paste0("Down-regulated: ", num_down),
      label.padding = unit(0.55, "lines"),
      label.size = 0.35,
      color = "blue",
      fill = NA) +
    geom_label(
      x = max(log2[log2 != Inf]) - (max(log2[log2 != Inf]) - min(log2[log2 != -Inf])) / 18, y = max(pv),
      label = paste0("Up-regulated: ", num_up),
      label.padding = unit(0.55, "lines"),
      label.size = 0.35,
      color = "red",
      fill = NA)

  return(list(plot = Vplot,
              Genes = list(UP = df$gene[df$color == "up-regulated"],
                           DOWN = df$gene[df$color == "down-regulated"])))
}
