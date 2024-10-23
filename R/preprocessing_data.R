#' Data Preprocessing Pipeline
#'
#' This function performs a series of preprocessing steps on a dataset, including removing duplicate genes, grouping data by specified groups, and normalizing the data.
#'
#' @param dataset A data frame containing the data to preprocess.
#' @param names_of_groups A character vector specifying the names of the groups for data grouping.
#' @param gene_column An integer or character specifying the column in the dataset containing gene names.
#' @param normType A character string specifying the normalization type to apply. See \code{\link{Normalization}} for the available normalization types.
#' @param ... Additional arguments passed to the functions \code{\link{remove_duplicates}}, \code{\link{group_listing}}, and \code{\link{Normalization}}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{data.unique}{A data frame with duplicates removed.}
#'   \item{data.grouped}{The dataset grouped by the specified groups, with frequency filtering applied.}
#'   \item{data.grouped.full}{The dataset grouped without frequency filtering.}
#'   \item{data.grouped.evenDim}{The dataset grouped with shared genes across all groups.}
#'   \item{data.norm}{The normalized dataset.}
#' }
#' @export
#'
#' @examples
#' # Example dataset
#' data.unique <- data.frame(Gene = paste0("Gene_",1:5),
#'                           Group1   = c(10, 20, 30, 56, 98),
#'                           Group1_1 = c(15, 25, 35, 11, 55),
#'                           Group2_1 = c(5, 15, 25, 654, 79),
#'                           Group2_2 = c(5, 123, 215, 36, 41),
#'                           Group3_1 = c(12,12, 12, 11, 87))
#'
#' # Preprocess data using different normalization methods
#' preprocessing_data(data.unique, names_of_groups = c("Group1", "Group2", "Group3"),
#'                    gene_column = "Gene", normType = "Znorm")
#'
preprocessing_data <- function(dataset, names_of_groups, gene_column, normType, ...) {
  args_list <- list(...)
  names(names_of_groups) <- names_of_groups

  args_remove_duplicates <- args_list[intersect(names(args_list),
                                                names(formals(remove_duplicates)))]

  data.unique <- suppressMessages(do.call(remove_duplicates,
                                          c(list(dataset = dataset,
                                                 genes = gene_column),
                                            args_remove_duplicates)))

  # Grouping Data
  args_group_listing <- args_list[intersect(names(args_list),
                                            names(formals(group_listing)))]
  data.grouped <- do.call(group_listing,
                          c(list(dataset = data.unique,
                                 names_of_groups = names_of_groups,
                                 freq = 1),
                            args_group_listing))

  data.grouped.full <- do.call(group_listing,
                               c(list(dataset = data.unique,
                                      names_of_groups = names_of_groups,
                                      freq = 0),
                                 args_group_listing))

  data.grouped.evenDim <- do.call(group_listing,
                                  c(list(dataset = data.unique,
                                         names_of_groups = names_of_groups,
                                         freq = 1,
                                         just_shared_genes = TRUE),
                                    args_group_listing))

  # Normalization
  if (!is.null(normType)){
    data.norm <- Normalization(data.unique, normType)
  } else {
    data.norm <- data.unique
  }


  return(list(data.unique = data.unique,
              data.grouped = data.grouped,
              data.grouped.full = data.grouped.full,
              data.grouped.evenDim = data.grouped.evenDim,
              data.norm = data.norm))
}
