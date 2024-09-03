#' FC
#'
#' @param list.groups A list of groups that we want to compare
#'
#' @return A matrix of FC indexes
#' @export
#'
FC <- function(list.groups){
  names_of_groups <- names(list.groups)
  names(names_of_groups) <- names_of_groups

  list_groups.mean <- sapply(list.groups,colMeans)

  names_col <- c()
  for (i in 1:(ncol(list_groups.mean)-1)){
    for (j in (i+1):ncol(list_groups.mean)){
      names_col <- c(names_col, paste0(colnames(list_groups.mean)[i],"_vs_",
                                      colnames(list_groups.mean)[j]))
      names_col <- c(names_col, paste0(colnames(list_groups.mean)[j],"_vs_",
                                      colnames(list_groups.mean)[i]))
    }
  }

  FC <-matrix(nrow = nrow(list_groups.mean), ncol = factorial(ncol(list_groups.mean)),
                              dimnames = list(rownames(list_groups.mean),names_col))

  n <- 1
  for (i in 1:(ncol(list_groups.mean)-1)){
    for (j in (i+1):ncol(list_groups.mean)){

      FC[,n] <- -log2(list_groups.mean[,i]/list_groups.mean[,j])
      n <- n+1

      FC[,n] <- -log2(list_groups.mean[,j]/list_groups.mean[,i])
      n <- n+1

    }
  }

  return(FC)
}
