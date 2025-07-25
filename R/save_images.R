#' Save MDS and Volcano plots from quantitative analysis
#'
#' @param quantitative_analysis_results A list containing MDS and volcano plots.
#' @param device The file format for saving plots (default: "svg").
#' @param width Final width of the saved images.
#' @param height Final height of the saved images.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' save_images(quantitative_analysis_results)
#' }
save_images <- function(quantitative_analysis_results, device = "svg", width = 16, height = 10){
  qar <- quantitative_analysis_results

  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)

  dir.create("MDS_plots")
  setwd("MDS_plots/")
  if (!is.null(qar[["mds_plot"]])){
    ggplot2::ggsave(qar[["mds_plot"]],
                    filename = paste0("generalMDS.",device),
                    device = device,
                    width = width,
                    height = height)
  }

  lapply(names(qar$mds_plot_pairw), function(x){
    if (!is.null(qar$mds_plot_pairw[[x]])){
      ggplot2::ggsave(qar$mds_plot_pairw[[x]],
                      filename = paste0("pairwiseMDS_",x,".",device),
                      device = device,
                      width = width,
                      height = height)
    }
  })
  setwd("..")
  dir.create("Volcano plots")
  setwd("Volcano plots/")

  lapply(names(qar$volcano_plot), function(x){
    if (!is.null(qar$volcano_plots[[x]])){
      ggplot2::ggsave(qar$volcano_plots[[x]],
                      filename = paste0("vPlot_",x,".",device),
                      device = device,
                      width = width,
                      height = height)
    }
  })
  setwd("..")
}
