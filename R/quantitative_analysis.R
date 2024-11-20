#' Quantitative Analysis of Dataset
#'
#' This function performs quantitative analysis on a dataset, including linear discriminant analysis (LDA), pairwise LDA (if more than two groups are provided), and calculation of additional indices such as DAve, DCI, and fold change.
#'
#' @param dataset A data frame containing the data to analyze.
#' @param names_of_groups A character vector specifying the names of the groups for analysis.
#' @param gene_column An integer or character specifying the column in the dataset containing gene names.
#' @param data.grouped.full List of data frames with the same number of rows.
#' @param ... Additional arguments passed to the underlying functions for LDA, DAve, DCI, and fold change calculations.
#' @param significance.LDA A numeric argument indicating the significance threshold for Linear Discriminant Analysis
#'
#' @return A list containing the results of the quantitative analysis:
#' \describe{
#'   \item{LDA_results}{The results from the linear discriminant analysis (LDA).}
#'   \item{LDA_pairw.results}{The results from pairwise LDA, if applicable.}
#'   \item{DAve_index}{The DAve index calculated from the data.}
#'   \item{DCI_index}{The DCI index calculated from the data.}
#'   \item{Fold_Change}{The fold change analysis results.}
#' }
#' @export
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#' matrix(runif(1500),ncol = 15,
#'        dimnames = list(paste0("Gene_",1:100),
#'                        paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
#' )
#' data$Gene <- paste0("Gene_",1:100)
#'
#' # Perform quantitative analysis
#' quantitative_analysis(data, names_of_groups = c("Group1", "Group2", "Group3"),
#'                       gene_column = "Gene", significance.LDA = 0.05)
#'
quantitative_analysis <- function(dataset, names_of_groups, gene_column, data.grouped.full = NULL,
                                  significance.LDA, ...) {
  args_list <- list(...)
  args_LDA <- args_list[intersect(names(args_list), names(formals(LDA)))]
  args_MDS <- args_list[intersect(names(args_list), names(formals(MDS_plot)))]
  # Perform LDA
  LDA_pairw.results <- do.call(LDA_pairwise,
                               c(list(dataset = dataset,
                                      names_of_groups = names_of_groups,
                                      gene_column = gene_column,
                                      significance.LDA = significance.LDA),
                                 args_LDA))

  # Perform pairwise LDA if more than two groups
  if (length(names_of_groups) > 2) {
    LDA_results <- do.call(LDA,
                           c(list(dataset = dataset,
                                  names_of_groups = names_of_groups,
                                  gene_column = gene_column,
                                  significance.LDA = significance.LDA),
                             args_LDA)
    )
    if (!is.null(LDA_results$dataset.LDA)){
      mds_plot <- do.call(MDS_plot,
                          c(list(dataset = LDA_results$dataset.LDA,
                                 names_of_groups = names_of_groups),
                            args_MDS)
    )
    } else {
      mds_plot    <- NULL
    }
  } else {
    LDA_results <- NULL
    mds_plot    <- NULL
  }

  # Create volcano plots and mds plots
  combinations_nog <- apply(combn(names_of_groups,2),2, c,simplify = F)
  mds_plot_pairw <- lapply(seq_along(combinations_nog), function(x){
    sign_feat <- sum(LDA_pairw.results[[x]]$features_p.values$p.adj < significance.LDA)
    if (sign_feat>0){
      do.call(MDS_plot,
              c(list(dataset = LDA_pairw.results[[x]]$dataset.LDA,
                     names_of_groups = combinations_nog[[x]]),
                args_MDS))

    }
  })
  names(mds_plot_pairw) <- names(LDA_pairw.results)
  data_volcano <- lapply(LDA_pairw.results, function(x) {
    rbind(x$dataset.LDA,
          x$features_p.values$p.adj[x$features_p.values$p.adj < significance.LDA])
  })

  args_volcanos  <- args_list[intersect(names(args_list), names(formals(volcano_groups)))]
  volcano_plots <- lapply(data_volcano, function(x){
    if (ncol(x)){
      do.call(volcano_groups,
              c(list(dataset.t = x,
                     names_of_groups = names_of_groups,
                     significance = significance.LDA),
                args_volcanos))
    }
  })


  # Calculate additional indices
  if (is.null(data.grouped.full)){
    args_group_listing <- args_list[intersect(names(args_list), names(formals(group_listing)))]

    data.grouped.full <- do.call(group_listing,
                                 c(list(dataset = dataset,
                                        names_of_groups = names_of_groups),
                                   args_group_listing))
  }

  DAve_index <- DAve(data.grouped.full)
  DCI_index <- DCI(data.grouped.full)
  Fold_Change <- FC(data.grouped.full)

  return(list(LDA_results       = LDA_results,
              LDA_pairw.results = LDA_pairw.results,
              DAve_index     = DAve_index,
              DCI_index      = DCI_index,
              Fold_Change    = Fold_Change,
              volcano_plots  = volcano_plots,
              mds_plot       = mds_plot,
              mds_plot_pairw = mds_plot_pairw
              ))
}
