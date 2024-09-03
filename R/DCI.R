#' DCI
#'
#' @param list.groups A list of groups that we want to compare
#'
#' @return A matrix of DCI indexes
#' @export
#'
DCI <- function(list.groups){
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
  DCI  <- matrix(nrow = nrow(list_groups.mean), ncol = factorial(ncol(list_groups.mean)),
                              dimnames = list(rownames(list_groups.mean),names_col))

  n <- 1
  for (i in 1:(ncol(list_groups.mean)-1)){
    for (j in (i+1):ncol(list_groups.mean)){

      DCI[,n] <- (list_groups.mean[,i]+list_groups.mean[,j])*
        (list_groups.mean[,i]-list_groups.mean[,j])/2
      n <- n+1

      DCI[,n] <- (list_groups.mean[,j]+list_groups.mean[,i])*
        (list_groups.mean[,j]-list_groups.mean[,i])/2
      n <- n+1

    }
  }

  return(DCI)
}
