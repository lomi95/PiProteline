#' Save MDS and Volcano plots from quantitative analysis
#'
#' @param quantitative_analysis_results A list containing MDS and volcano plots.
#' @param device The file format for saving plots (default: "svg").
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' save_images(quantitative_analysis_results)
#' }
save_images <- function(quantitative_analysis_results, device = "svg"){
  qar <- quantitative_analysis_results

  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)

  dir.create("MDS_plots")
  setwd("MDS_plots/")
  if (!is.null(qar$mds_plot)){
    ggplot2::ggsave(qar$mds_plot, filename = paste0("generalMDS.",device), device = device)
  }

  lapply(names(qar$mds_plot_pairw), function(x){
    if (!is.null(qar$mds_plot_pairw[[x]])){
      ggplot2::ggsave(qar$mds_plot_pairw[[x]], filename = paste0("pairwiseMDS_",x,".",device), device = device)
    }
  })
  setwd("..")
  dir.create("Volcano plots")
  setwd("Volcano plots/")

  lapply(names(qar$volcano_plot), function(x){
    if (!is.null(qar$volcano_plots[[x]])){
      ggplot2::ggsave(qar$volcano_plots[[x]], filename = paste0("vPlot_",x,".",device), device = device)
    }
  })
  setwd("..")
}
