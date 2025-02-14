#' Group Listing Function
#'
#' This function groups rows of a dataset based on user-specified conditions or index positions. It allows for flexible grouping of columns, either based on matching patterns in column names or on specified column index vectors.
#'
#' @param dataset A data frame where rows represent proteins and columns represent samples.
#' @param names_of_groups A character vector specifying the conditions within `colnames(dataset)` to group by. Ignored if `pos_vectors_groups` is not `NULL`.
#' @param gene_column An integer or character specifying the column in the dataset containing gene names, that will be used as rownames. If `NULL` (default)
#' @param ignore_case A logical value indicating whether the matching of `names_of_groups` should be case-insensitive. Default is `TRUE`. Ignored if `pos_vectors_groups` is provided.
#' @param pos_vectors_groups A list where each element is a vector of column indices. If provided, `names_of_groups` and `ignore_case` are ignored.
#' @param freq A numeric value between 0 and 1 indicating the minimum percentage of non-zero or non-NA values for a protein to be retained in the groups.
#' @param just_shared_genes A logical value indicating whether only the genes shared across all groups should be retained. Default is `FALSE`.
#'
#' @importFrom stats aggregate
#' @return A list where each element is a subset of the dataset corresponding to a specific group of columns.
#'
#' @examples
#' # Example 1: Using names_of_groups to group columns
#' dataset <- matrix(rnorm(100), nrow = 10)
#' colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2",
#'                        "control_3", "treatment_3", "control_4", "control_5",
#'                        "treatment_4", "treatment_5")
#' group_listing(dataset, names_of_groups = c("control", "treatment"))
#'
#' # Example 2: Using pos_vectors_groups to group columns
#' group_listing(dataset, pos_vectors_groups = list(c(1, 3, 5), c(2, 4, 6)))
#'
#' @export
group_listing <- function(dataset,
                          names_of_groups = NULL,
                          gene_column = NULL,
                          ignore_case = TRUE,
                          pos_vectors_groups = NULL,
                          freq = 0,
                          just_shared_genes = F){
  old_opt <- options()$warn
  options(warn = 1)
  on.exit(options(warn = old_opt))

  if (!is.null(gene_column)){
    rownames(dataset) <- dataset[,gene_column]
  }

  if (is.null(pos_vectors_groups)){
    if (is.null(names_of_groups)){
      stop("Both `names_of_groups` and `pos_vectors_groups` are missing, with no default")
    }
    if (length(names_of_groups)<2){
      stop("`names_of_groups` should be a character string of length at least 2")
    }
    names(names_of_groups) <- names_of_groups

    indxs <- table(unlist(lapply(names_of_groups, function(x) {
      grep(x,colnames(dataset), ignore.case = ignore_case)
    })))
    if (max(indxs)>1){
      warning(paste(rownames(dataset)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
    }

    list.groups <- lapply(names_of_groups, function(x){
      dataset[,grep(x,colnames(dataset), ignore.case = ignore_case),drop = F]
    })

  } else if (is.list(pos_vectors_groups)){
    if (length(pos_vectors_groups)<2){
      stop("`pos_vectors_groups` should be a list of length at least 2")
    }
    indxs <- table(unlist(pos_vectors_groups))
    if (max(indxs)>1){
      warning(paste(rownames(dataset)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
    }
    if (is.null(names(pos_vectors_groups))){
      names(pos_vectors_groups) <- paste("Group", seq(1,length(pos_vectors_groups)), sep = "_")
    }
    list.groups <- lapply(pos_vectors_groups, function(x){
      if(sum(x>ncol(dataset))){
        x <- x[!(x>nrow(dataset))]
        warning("One or more elements are out of dataset dimension limits, they will be removed")
      }
      dataset[,x,drop = F]
    })

    if (sum(sapply(list.groups,ncol)==0)){
      warning( paste(names(which(sapply(list.groups,ncol)==0)), collapse = ", "), " was/were not found")
      list.groups <- list.groups[sapply(list.groups,ncol)!=0]
    }

  }  else {
    stop("'pos_vectors_groups' should be a list in which each element is a vector containing the column indexes")
  }

  list.groups <- lapply(list.groups, function(x){
    x[apply(x, 1, function(y) 1 - sum(y == 0 | is.na(y))/length(y) >= freq),,drop = F]
  })

  if (just_shared_genes){
    sg <- Reduce(intersect,lapply(list.groups, rownames))
    list.groups  <- lapply(list.groups, function(x) x[sg,,drop = F])
  }
  return(list.groups)
}
