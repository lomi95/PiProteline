#' DCI Index Calculation for Pairwise Group Comparisons
#'
#' Computes the DCI (Discriminant Component Index) for pairwise comparisons between groups of observations.
#'
#' @param groups_list A list where each element represents a group of observations (e.g., numeric vectors or matrices of data).
#'
#' @return A matrix where each column corresponds to the DCI index for pairwise comparisons between groups.
#' The column names follow the pattern "Group1_vs_Group2" and "Group2_vs_Group1" for each pairwise comparison.
#' @references De Palma A, Agresta AM, Viglio S, Rossi R, D'Amato M, Di Silvestre D, Mauri P, Iadarola P.
#'     A Shotgun Proteomic Platform for a Global Mapping of Lymphoblastoid Cells to Gain Insight into Nasu-Hakola Disease.
#'     Int J Mol Sci. 2021 Sep 15;22(18):9959. doi: 10.3390/ijms22189959. PMID: 34576123; PMCID: PMC8472724.
#'     (https://www.mdpi.com/1422-0067/22/18/9959)
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
#' DCI(groups_list)
#'
#' @export
DCI <- function(groups_list){

  names_of_groups <- names(groups_list)
  names(names_of_groups) <- names_of_groups

  list_groups.mean <- sapply(groups_list, function(x){
    rM <- rowSums(x)/ncol(x)
    rM[is.na(rM)] <- 0
    return(rM)
  })

  names_col <- c()
  for (i in 1:(ncol(list_groups.mean) - 1)){
    for (j in (i + 1):ncol(list_groups.mean)){
      names_col <- c(names_col, paste0(colnames(list_groups.mean)[i], "_vs_", colnames(list_groups.mean)[j]))
    }
  }

  DCI <- matrix(nrow = nrow(list_groups.mean), ncol = length(names_col),
                 dimnames = list(rownames(list_groups.mean), names_col))

  n <- 1
  for (i in 1:(ncol(list_groups.mean) - 1)){
    for (j in (i + 1):ncol(list_groups.mean)){
      DCI[,n] <- (list_groups.mean[,i]+list_groups.mean[,j])*
        (list_groups.mean[,i]-list_groups.mean[,j])/2
      n <- n+1
    }
  }


  return(data.frame(GeneName = rownames(DCI), DCI))
}
