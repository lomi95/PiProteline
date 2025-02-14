#' Fold Change (FC) Index Calculation for Pairwise Group Comparisons
#'
#' Computes the Fold Change (FC) for pairwise comparisons between groups of observations.
#'
#' @param groups_list A list where each element represents a group of observations (e.g., numeric vectors or matrices of data).
#'
#' @return A matrix where each column corresponds to the log2 Fold Change (FC) for pairwise comparisons between groups.
#' The column names follow the pattern "Group1_vs_Group2" and "Group2_vs_Group1" for each pairwise comparison.
#'
#' @examples
#' group1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' group2 <- matrix(c(3, 2, 1, 6, 5, 4), nrow = 2)
#' group3 <- matrix(c(5, 4, 3, 2, 1, 0), nrow = 2)
#' groups_list <- list(Group1 = group1, Group2 = group2, Group3 = group3)
#' groups_list <- lapply(groups_list, function(x){
#'     rownames(x) <- c("gene1","gene2")
#'     colnames(x) <- paste0("sample_",1:3)
#'     return(x)
#' })
#' FC(groups_list)
#'
#' @export
FC <- function(groups_list){


  names_of_groups <- names(groups_list)
  names(names_of_groups) <- names_of_groups

  list_groups.mean <- sapply(groups_list, function(x){
    rM <- rowMeans(x)
    rM[is.na(rM)] <- 0
    return(rM)
  })

  names_col <- c()
  for (i in 1:(ncol(list_groups.mean) - 1)){
    for (j in (i + 1):ncol(list_groups.mean)){
      names_col <- c(names_col, paste0(colnames(list_groups.mean)[i], "_vs_", colnames(list_groups.mean)[j]))
    }
  }

  FC <-matrix(nrow = nrow(list_groups.mean), ncol = length(names_col),
              dimnames = list(rownames(list_groups.mean),names_col))

  n <- 1
  for (i in 1:(ncol(list_groups.mean) - 1)){
    for (j in (i + 1):ncol(list_groups.mean)){
      FC[,n] <- log2(list_groups.mean[,i]/list_groups.mean[,j])
      n <- n+1
    }
  }

  return(data.frame(GeneName = rownames(FC), FC))
}
