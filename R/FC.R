#' Fold Change (FC) Index Calculation for Pairwise Group Comparisons
#'
#' Computes the Fold Change (FC) for pairwise comparisons between groups of observations.
#'
#' @param list.groups A list where each element represents a group of observations (e.g., numeric vectors or matrices of data).
#'
#' @return A matrix where each column corresponds to the log2 Fold Change (FC) for pairwise comparisons between groups.
#' The column names follow the pattern "Group1_vs_Group2" and "Group2_vs_Group1" for each pairwise comparison.
#'
#' @examples
#' group1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' group2 <- matrix(c(3, 2, 1, 6, 5, 4), nrow = 2)
#' group3 <- matrix(c(5, 4, 3, 2, 1, 0), nrow = 2)
#' list.groups <- list(Group1 = group1, Group2 = group2, Group3 = group3)
#' FC(list.groups)
#'
#' @export
FC <- function(list.groups){


  names_of_groups <- names(list.groups)
  names(names_of_groups) <- names_of_groups

  list_groups.mean <- sapply(list.groups, rowMeans)

  names_col <- c()
  for (i in 1:(ncol(list_groups.mean) - 1)){
    for (j in (i + 1):ncol(list_groups.mean)){
      names_col <- c(names_col, paste0(colnames(list_groups.mean)[i], "_vs_", colnames(list_groups.mean)[j]))
      names_col <- c(names_col, paste0(colnames(list_groups.mean)[j], "_vs_", colnames(list_groups.mean)[i]))
    }
  }

  FC <-matrix(nrow = nrow(list_groups.mean), ncol = factorial(ncol(list_groups.mean)),
              dimnames = list(rownames(list_groups.mean),names_col))

  n <- 1
  for (i in 1:(ncol(list_groups.mean) - 1)){
    for (j in (i + 1):ncol(list_groups.mean)){
      FC[,n] <- -log2(list_groups.mean[,i]/list_groups.mean[,j])
      n <- n+1

      FC[,n] <- -log2(list_groups.mean[,j]/list_groups.mean[,i])
      n <- n+1

    }
  }

  return(FC)
}
