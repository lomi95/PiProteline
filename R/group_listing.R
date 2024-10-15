#' Group Listing Function
#'
#' This function groups rows of a dataset based on user-specified conditions or index positions.
#'
#' @param dataset A matrix or data frame where rows represent proteins and columns represent samples.
#' @param names_of_groups A character vector specifying the conditions within `rownames(dataset)` to group by. Ignored if `pos.vectors_groups` is not NULL.
#' @param ignoreCase A logical value indicating whether the matching of `names_of_groups` should be case-insensitive. Default is TRUE. Ignored if `pos.vectors_groups` is provided.
#' @param pos.vectors_groups A list where each element is a vector of row indices. If provided, `names_of_groups` and `ignoreCase` are ignored.
#'
#' @importFrom stats aggregate
#'
#' @return A list where each element is a subset of the dataset corresponding to a specific group of rows.
#'
#' If `names_of_groups` is provided, rows will be grouped based on matching patterns in the row names of the dataset.
#' If `pos.vectors_groups` is provided, rows will be grouped based on the index vectors.
#'
#' Warnings:
#' - If any row is selected for more than one group, a warning will be issued.
#' - If some indices in `pos.vectors_groups` exceed the dimensions of the dataset, they will be removed, and a warning will be issued.
#' - If any group is empty, a warning will be issued, and the empty groups will be removed.
#'
#' @examples
#' # Example 1: Using names_of_groups to group rows
#' dataset <- matrix(rnorm(100), nrow = 10)
#' rownames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2",
#'                        "control_3", "treatment_3", "control_4", "control_5",
#'                        "treatment_4", "treatment_5")
#' group_listing(dataset, names_of_groups = c("control", "treatment"))
#'
#' # Example 2: Using pos.vectors_groups to group rows
#' group_listing(dataset, pos.vectors_groups = list(c(1, 3, 5), c(2, 4, 6)))
#'
#' @export
group_listing <- function(dataset,
                          names_of_groups = NULL,
                          ignoreCase = TRUE,
                          pos.vectors_groups = NULL){
  old_opt <- options()$warn
  options(warn = 1)
  on.exit(options(warn = old_opt))

  if (is.null(pos.vectors_groups)){
    if (is.null(names_of_groups)){
      stop("Both `names_of_groups` and `pos.vectors_groups` are missing, with no default")
    }
    if (length(names_of_groups)<2){
      stop("`names_of_groups` should be a character string of length at least 2")
    }
    names(names_of_groups) <- names_of_groups

    indxs <- table(unlist(lapply(names_of_groups, function(x) {
      grep(x,rownames(dataset), ignore.case = ignoreCase)
    })))
    if (max(indxs)>1){
      warning(paste(colnames(dataset)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
    }

    list.groups <- lapply(names_of_groups, function(x){
      dataset[grep(x,rownames(dataset), ignore.case = ignoreCase),]
    })

  } else if (is.list(pos.vectors_groups)){
    if (length(pos.vectors_groups)<2){
      stop("`pos.vectors_groups` should be a list of length at least 2")
    }
    indxs <- table(unlist(pos.vectors_groups))
    if (max(indxs)>1){
      warning(paste(colnames(dataset)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
    }
    if (is.null(names(pos.vectors_groups))){
      names(pos.vectors_groups) <- paste("Group", seq(1,length(pos.vectors_groups)), sep = "_")
    }
    list.groups <- lapply(pos.vectors_groups, function(x){
      if(sum(x>nrow(dataset))){
        x <- x[!(x>ncol(dataset))]
        warning("One or more elements are out of dataset dimension limits, they will be removed")
      }
      dataset[x,,drop = F]
    })

    if (sum(sapply(list.groups,nrow)==0)){
      warning( paste(names(which(sapply(list.groups,nrow)==0)), collapse = ", "), " was/were not found")
      list.groups <- list.groups[sapply(list.groups,nrow)!=0]
    }

  }  else {
    stop("'pos.vectors_groups' should be a list in which each element is a vector containing the column indexes")
  }
  return(list.groups)
}
