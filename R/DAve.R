#' DAve Index Calculation
#'
#' Computes the Differential Average (DAve) index for each pair of groups, allowing the comparison of the mean values
#' across multiple groups.
#'
#' @param groups_list A list where each element is a numeric matrix or data frame representing a group.
#'     The columns represent different variables, and the rows represent samples.
#'
#' @details The DAve index is calculated for all possible pairwise comparisons between the groups in the list.
#'     For a pair of groups \(i\) and \(j\), the DAve index for a variable is calculated as:
#'     \deqn{DAve_{ij} = \frac{(mean_i - mean_j)}{(mean_i + mean_j)/0.5}}
#'     where \(mean_i\) and \(mean_j\) are the column-wise means for groups \(i\) and \(j\), respectively.
#'
#' @return A matrix of DAve indexes. The rows correspond to the variables, and the columns represent the pairwise
#'     comparisons between groups.
#' @references De Palma A, Agresta AM, Viglio S, Rossi R, D'Amato M, Di Silvestre D, Mauri P, Iadarola P.
#'     A Shotgun Proteomic Platform for a Global Mapping of Lymphoblastoid Cells to Gain Insight into Nasu-Hakola Disease.
#'     Int J Mol Sci. 2021 Sep 15;22(18):9959. doi: 10.3390/ijms22189959. PMID: 34576123; PMCID: PMC8472724.
#'     (https://www.mdpi.com/1422-0067/22/18/9959)
#' @examples
#' # Example dataset with two groups
#' group1 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' group2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' group3 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' groups_list <- list(Group1 = group1, Group2 = group2, Group3 = group3)
#' groups_list <- lapply(groups_list, function(x){
#'     rownames(x) <- c("gene1","gene2","gene3","gene4","gene5")
#'     colnames(x) <- paste0("sample_",1:4)
#'     return(x)
#' })
#'
#' # Calculate DAve
#' DAve_result <- DAve(groups_list)
#' print(DAve_result)
#'
#' @export
DAve <- function(groups_list){
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

  DAve <- matrix(nrow = nrow(list_groups.mean), ncol = length(names_col),
                 dimnames = list(rownames(list_groups.mean), names_col))

  n <- 1
  for (i in 1:(ncol(list_groups.mean) - 1)){
    for (j in (i + 1):ncol(list_groups.mean)){
      DAve[, n] <- ((list_groups.mean[, i] - list_groups.mean[, j]) /
        (list_groups.mean[, i] + list_groups.mean[, j]) )/ 0.5
      n <- n + 1
    }
  }

  return(data.frame(GeneName = rownames(DAve), DAve))
}
