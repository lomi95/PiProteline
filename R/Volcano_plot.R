#' Volcano Plot
#'
#' @param df Data frame with columns named log2 (x axis), pv (y axis), gene (labels)
#' @param boundFC Numeric vector. Interval out of which labels are highlithed in red and labels are shown
#' @param Name_1 character Name of group 1
#' @param Name_2 character Name of group 2
#' @param thr_pvalue numeric value. Value under of which labels are highlithed in red and labels are shown
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_label
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 unit
#'
#' @return A ggplot object
#' @export
#'
Volcano_Plot <- function(df,boundFC,thr_pvalue, Name_1 = "Group1", Name_2 = "Group2"){
  thr_pvalue.log <- -log10(thr_pvalue)
  log2 <- df$log2
  pv   <- df$pv
  df$color <- "Non-significant"
  df$color[log2 >= max(boundFC) & pv >= thr_pvalue.log] <- "up-regulated"
  df$color[log2 <= min(boundFC) & pv >= thr_pvalue.log] <- "down-regulated"
  df$color <- as.factor(df$color)

  df$gene[df$color == "Non-significant"] <- ""

  num_up <- sum(df$color == "up-regulated")
  num_down <- sum(df$color == "down-regulated")

  Vplot <- ggplot(df, aes(x = log2, y = pv)) +
    geom_point(aes(color = color)) +

    scale_color_manual(values = c("up-regulated"    = "red",
                                  "down-regulated"  = "blue",
                                  "Non-significant" = "grey")) +

    geom_vline(xintercept = min(boundFC),   linetype = "dashed", color = "black") +
    geom_vline(xintercept = max(boundFC),   linetype = "dashed", color = "black") +
    geom_hline(yintercept = thr_pvalue.log, linetype = "dashed", color = "black") +
    geom_text_repel(aes(label = gene),
                    hjust = -0.2, vjust = -0.2,
                    box.padding = 0.35, point.padding = 0.3,
                    segment.color = 'grey50',
                    max.overlaps = 5) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = paste0("Volcano Plot - UP ", Name_1," vs DOWN ",Name_2),
         x = "log2(FC)",
         y = "-log10(p.adjusted)") +
    geom_label(
      x = min(log2) + (max(log2)- min(log2))/18, y = max(pv), label = paste0("Down-regulated: ",num_down),
      label.padding = unit(0.55, "lines"),
      label.size = 0.35,
      color = "blue",
      fill = NA) +
    geom_label(
      x = max(log2) - (max(log2)- min(log2))/18, y = max(pv), label = paste0("Up-regulated: ",num_up),
      label.padding = unit(0.55, "lines"),
      label.size = 0.35,
      color = "red",
      fill = NA)

  return(list(plot = Vplot,
              Genes = list(UP   = df$gene[df$color == "up-regulated"],
                           DOWN = df$gene[df$color == "down-regulated"])))
}

