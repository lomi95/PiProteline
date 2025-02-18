#' Data Preprocessing Pipeline
#'
#' This function performs a series of preprocessing steps on a dataset, including removing duplicate genes, grouping data by specified groups, and normalizing the data.
#'
#' @param dataset A data frame containing the data to preprocess.
#' @param names_of_groups A character vector specifying the names of the groups for data grouping.
#' @param gene_column An integer or character specifying the column in the dataset containing gene names.
#' @param norm_type A character string specifying the normalization type to apply. See \code{\link{normalization}} for the available normalization types.
#' @param ... Additional arguments passed to the functions \code{\link{remove_duplicates}}, \code{\link{group_listing}}, and \code{\link{normalization}}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{data_unique}{A data frame with duplicates removed.}
#'   \item{data_grouped}{The dataset grouped by the specified groups, with frequency filtering applied.}
#'   \item{data_grouped_full}{The dataset grouped without frequency filtering.}
#'   \item{data_grouped_even_dim}{The dataset grouped with shared genes across all groups.}
#'   \item{data_norm}{The normalized dataset.}
#' }
#' @export
#'
#' @examples
#' # Example dataset
#' data_unique <- data.frame(Gene = paste0("Gene_",1:5),
#'                           Group1   = c(10, 20, 30, 56, 98),
#'                           Group1_1 = c(15, 25, 35, 11, 55),
#'                           Group2_1 = c(5, 15, 25, 654, 79),
#'                           Group2_2 = c(5, 123, 215, 36, 41),
#'                           Group3_1 = c(12,12, 12, 11, 87))
#'
#' # Preprocess data using different normalization methods
#' preprocessing_data(data_unique, names_of_groups = c("Group1", "Group2", "Group3"),
#'                    gene_column = "Gene", norm_type = "Znorm")
#'
preprocessing_data <- function(dataset, names_of_groups, gene_column, norm_type, ...) {

  args_list <- list(...)

  names(names_of_groups) <- names_of_groups

  args_remove_duplicates <- args_list[intersect(names(args_list),
                                                names(formals(remove_duplicates)))]

  data_unique <- suppressMessages(do.call(remove_duplicates,
                                          c(list(dataset = dataset,
                                                 genes = gene_column),
                                            args_remove_duplicates)))

  # Grouping Data
  args_group_listing <- args_list[intersect(names(args_list),
                                            names(formals(group_listing)))]
  data_grouped <- do.call(group_listing,
                          c(list(dataset = data_unique,
                                 names_of_groups = names_of_groups,
                                 freq = 1),
                            args_group_listing))

  data_grouped_full <- do.call(group_listing,
                               c(list(dataset = data_unique,
                                      names_of_groups = names_of_groups,
                                      freq = 0),
                                 args_group_listing))

  data_grouped_even_dim <- do.call(group_listing,
                                  c(list(dataset = data_unique,
                                         names_of_groups = names_of_groups,
                                         freq = 1,
                                         just_shared_genes = TRUE),
                                    args_group_listing))

  # Normalization
  if (!is.null(norm_type)){
    data_norm <- normalization(data_unique, norm_type)
  } else {
    data_norm <- data_unique
  }


  return(list(data_unique = data_unique,
              data_grouped = data_grouped,
              data_grouped_full = data_grouped_full,
              data_grouped_even_dim = data_grouped_even_dim,
              data_norm = data_norm))
}
