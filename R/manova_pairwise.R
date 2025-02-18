#' Pairwise Multivariate Analysis of Variance (manova)
#'
#' This function performs pairwise Multivariate Analysis of Variance (manova) for all possible combinations of the specified groups in the dataset. It returns a list of manova results for each pairwise comparison.
#'
#' @param dataset A data frame containing the data for the analysis.
#' @param names_of_groups A character vector specifying the names of the groups to be compared.
#' @param gene_column The name or index of the column in the dataset that contains the gene identifiers.
#' @param ... Additional arguments to be passed to the `manova` function.
#'
#' @importFrom utils combn
#' @return A named list of manova results for each pairwise comparison, with names indicating the groups compared.
#' @export
#'
#' @examples
#' # Example dataset
#' set.seed(123)
#' data <- data.frame(
#'   gene = paste0("gene_", 1:10),
#'     matrix(rnorm(90), ncol = 9)
#'     )
#' colnames(data)[2:10] <- paste0("group", rep(1:3, each = 3),
#'                                "_", rep(1:3, times = 3))
#' # Perform pairwise manova
#' manova_results <- manova_pairwise(data,
#'                             names_of_groups = c("Group1", "Group2", "Group3"),
#'                             gene_column = 1)
#' print(manova_results)
manova_pairwise <- function(dataset, names_of_groups, gene_column, ...) {

  args_list <- list(...)
  args_manova <- args_list[intersect(names(args_list), names(formals(manova)))]

  combinations <- utils::combn(names_of_groups, 2)

  manova.pairwise <- suppressMessages(apply(combinations, 2, function(x) {
    do.call(manova, c(list(dataset = dataset,
                           names_of_groups = x,
                           gene_column = gene_column),
                      args_manova))
  }))
  names(manova.pairwise) <- apply(combinations, 2, paste, collapse = "_vs_")

  return(manova.pairwise)
}
