#' Correlation Between Groups
#'
#' This function computes the correlation between the means of different groups in a list of datasets. Each dataset represents a group, and pairwise correlations are computed for all possible combinations of groups.
#'
#' @param data_grouped A list of data frames or matrices, where each element represents a group. Rows represent observations, and columns represent variables within each group.
#' @param corr_groups A character string specifying the type of correlation to compute. Options include "pearson", "spearman". Default is "spearman".
#'
#' @importFrom Hmisc rcorr
#' @importFrom utils combn
#'
#' @return A named list of correlation results for each pair of groups, where each element contains the correlation matrix and associated statistics (p-values, etc.).
#' @examples
#' # Example datasets
#' group1 <- matrix(rnorm(100), nrow = 10)
#' group2 <- matrix(rnorm(100), nrow = 10)
#' group3 <- matrix(rnorm(100), nrow = 10)
#'
#' # Combine into a list
#' data_grouped <- list(Group1 = group1, Group2 = group2, Group3 = group3)
#'
#' # Compute correlations between the groups
#' corr_between_groups(data_grouped, corr_groups = "spearman")
#'
#' @export

corr_between_groups <- function(data_grouped, corr_groups = "spearman"){
  if (!is.list(data_grouped)){
    stop("'data_grouped' must be a list")
  }
  if (is.null(names(data_grouped))){
    names(data_grouped) <- paste0("Group_",1:length(data_grouped))
  }


  combinations <- utils::combn(names(data_grouped),2)
  CorrBetweenGroups <- apply(combinations,2, function(x){
    Hmisc::rcorr(rowMeans(data_grouped[[x[1]]]),rowMeans(data_grouped[[x[2]]]),type = corr_groups)
  })
  names(CorrBetweenGroups) <- apply(combinations, 2, paste0, collapse = "_vs_")

  return(CorrBetweenGroups)
}
