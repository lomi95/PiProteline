#' Quantitative Analysis of Dataset
#'
#' This function performs quantitative analysis on a dataset, including linear discriminant analysis (LDA), pairwise LDA (if more than two groups are provided), and calculation of additional indices such as DAve, DCI, and fold change.
#'
#' @param dataset A data frame containing the data to analyze.
#' @param names_of_groups A character vector specifying the names of the groups for analysis.
#' @param gene_column An integer or character specifying the column in the dataset containing gene names.
#' @param data.grouped.full List of data frames with the same number of rows.
#' @param ... Additional arguments passed to the underlying functions for LDA, DAve, DCI, and fold change calculations.
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
#'                       gene_column = "Gene")
#'
quantitative_analysis <- function(dataset, names_of_groups, gene_column, data.grouped.full = NULL, ...) {
  args_list <- list(...)
  args_LDA <- args_list[intersect(names(args_list), names(formals(LDA)))]

  # Perform LDA
  LDA_pairw.results <- do.call(LDA_pairwise,
                               c(list(dataset = dataset,
                                      names_of_groups = names_of_groups,
                                      gene_column = gene_column),
                                 args_LDA))

  # Perform pairwise LDA if more than two groups
  if (length(names_of_groups) > 2) {
    LDA_results <- do.call(LDA,
                           c(list(dataset = dataset,
                                  names_of_groups = names_of_groups,
                                  gene_column = gene_column),
                             args_LDA))
  } else {
    LDA_results <- NULL
  }

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
              DAve_index  = DAve_index,
              DCI_index   = DCI_index,
              Fold_Change = Fold_Change))
}
