#' Quantitative Analysis of Dataset
#'
#' This function performs quantitative analysis on a dataset, including Multivariate Analysis of Variance (manova), pairwise manova (if more than two groups are provided), and calculation of additional indices such as DAve, DCI, and fold change.
#'
#' @param dataset A data frame containing the data to analyze.
#' @param names_of_groups A character vector specifying the names of the groups for analysis.
#' @param gene_column An integer or character specifying the column in the dataset containing gene names.
#' @param data_grouped_full List of data frames with the same number of rows.
#' @param ... Additional arguments passed to the underlying functions for manova, DAve, DCI, and fold change calculations.
#' @param significance_manova A numeric argument indicating the significance threshold for Multivariate Analysis of Variance
#'
#' @return A list containing the results of the quantitative analysis:
#' \describe{
#'   \item{manova_results}{The results from manova analysis if more than two groups are present, with DAve and Fold Change indexes.}
#'   \item{manova_pairw_results}{The results from pairwise manova, with DAve and Fold Change indexes.}
#'   \item{mds_plot}{The results from Multi Dimensional Scaling if more than two groups are present.}
#'   \item{mds_plot_results}{The results from pairwise Multi dimensional Scaling.}
#'   \item{volcano_plots}{Volcano plot for each pairwise comparison.}
#' }
#' @export
#'
#' @importFrom stringr str_split
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
#' qa <- quantitative_analysis(data, names_of_groups = c("Group1", "Group2", "Group3"),
#'                             gene_column = "Gene", significance_manova = 0.05)
#'
quantitative_analysis <- function(dataset, names_of_groups, gene_column, data_grouped_full = NULL,
                                  significance_manova, ...) {
  args_list <- list(...)

  if (is.character(gene_column)){
    gene_column <- which(colnames(dataset) == gene_column)
  }

  colnames(dataset)[gene_column] <- "GeneName"

  args_manova <- args_list[intersect(names(args_list), names(formals(manova)))]
  args_MDS <- args_list[intersect(names(args_list), names(formals(MDS_plot)))]
  # Perform manova
  manova_pairw_results <- manova_pairwise(dataset = dataset,
                                          names_of_groups = names_of_groups,
                                          gene_column = gene_column,
                                          ... = ...)

  # Perform pairwise manova if more than two groups
  if (length(names_of_groups) > 2) {
    manova_results <- do.call(manova,
                              c(list(dataset = dataset,
                                   names_of_groups = names_of_groups,
                                   gene_column = gene_column),
                              args_manova))
    sign_feat <- manova_results$GeneName[manova_results$p.adj <= significance_manova]
    if (length(sign_feat)){

      dataManova <- dataset[dataset[,gene_column] %in% sign_feat,, drop = F]
      mds_plot <- do.call(MDS_plot,
                          c(list(t_dataset = transpose(dataManova,1),
                                 names_of_groups = names_of_groups),
                            args_MDS))

    } else {
      mds_plot    <- NULL
    }
  }

  # Create volcano plots and mds plots
  combinations_nog <- apply(combn(names_of_groups,2),2, c,simplify = F)
  mds_plot_pairw <- lapply(seq_along(combinations_nog), function(x){

    sign_feat <- manova_pairw_results[[x]]$GeneName[manova_pairw_results[[x]]$p.adj <= significance_manova]
    if (length(sign_feat)){
      dataManova <- dataset[dataset[,gene_column] %in% sign_feat,, drop = F]

      do.call(MDS_plot,
              c(list(t_dataset = transpose(dataManova,1),
                     names_of_groups = combinations_nog[[x]]),
                args_MDS))
    }
  })
  names(mds_plot_pairw) <- names(manova_pairw_results)

  dataVolcano <- data_volcano(dataset,manova_pairw_results,significance_manova)

  args_volcanos  <- args_list[intersect(names(args_list), names(formals(data_volcano)))]
  volcano_plots <- lapply(names(dataVolcano), function(x){
    if (ncol(dataVolcano[[x]])){
      g1vs2 <- stringr::str_split(x, "_vs_")[[1]]
      do.call(volcano_plot,c(list(t_dataset = dataVolcano[[x]],
                                  group_1 = g1vs2[1],
                                  group_2 = g1vs2[2],
                                  significance = significance_manova),
                             args_volcanos))
    }
  })
  names(volcano_plots) <- names(dataVolcano)

  # Calculate additional indices
  if (is.null(data_grouped_full)){
    args_group_listing <- args_list[intersect(names(args_list), names(formals(group_listing)))]

    data_grouped_full <- do.call(group_listing,c(list(dataset = dataset,
                                                      names_of_groups = names_of_groups),
                                                 args_group_listing))
  }


  if (length(names_of_groups) > 2){
    return(list(manova_results       = manova_results,
                manova_pairw_results = manova_pairw_results,
                volcano_plots  = volcano_plots,
                mds_plot       = mds_plot,
                mds_plot_pairw = mds_plot_pairw
    ))
  } else {
    return(list(manova_pairw_results = manova_pairw_results,
                volcano_plots  = volcano_plots,
                mds_plot_pairw = mds_plot_pairw
    ))
  }

}
