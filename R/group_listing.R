#' group.listing
#'
#' @param dataset.t Dataset Samples x Proteins
#' @param names_of_groups ignored if pos.vectors_groups is not NULL. Names of condition specified within rownames(dataset)
#' @param ignoreCase ignored if 'names_of_groups' is not used. If FALSE the names in 'names_of_groups' are case sensitives. default T.
#' @param pos.vectors_groups list with in each element a vector with the row indexes
#'
#' @importFrom stats aggregate
#'
#' @return A list with each specified group
#' @export
#'
group_listing <- function(dataset.t,
                          names_of_groups,
                          ignoreCase = T,
                          pos.vectors_groups = NULL){

  if (is.null(pos.vectors_groups)){
    names(names_of_groups) <- names_of_groups

    indxs <- table(unlist(lapply(names_of_groups, function(x) {
      grep(x,rownames(dataset.t), ignore.case = ignoreCase)
    })))
    if (max(indxs)>1){
      message("Warning: ",paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),"is/are selected for more than one group")
    }

    list.groups <- lapply(names_of_groups, function(x){
      dataset.t[grep(x,rownames(dataset.t), ignore.case = ignoreCase),]
    })


  } else if (is.list(pos.vectors_groups)){
    indxs <- table(unlist(pos.vectors_groups))
    if (max(indxs)>1){
      message("Warning: ",paste(rownames(dataset.t)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
    }
    if (is.null(names(pos.vectors_groups))){
      names(pos.vectors_groups) <- paste("Group", seq(1,length(pos.vectors_groups)), sep = "_")
    }
    list.groups <- lapply(pos.vectors_groups, function(x){
      if(sum(x>ncol(dataset.t))){
        x <- x[!(x>ncol(dataset.t))]
        message("Warning: One or more elements are out of dataset dimension limits, they will be removed")

      }
      dataset.t[x,]

    })

    if (sum(sapply(list.groups,nrow)==0)){
      message("Warning: ", paste(names(which(sapply(list.groups,nrow)==0)), collapse = ", "), " was/were not found")
      list.groups <- list.groups[sapply(list.groups,nrow)!=0]
    }

  }  else {
    stop("'pos.vectors_groups' should be a list in which each element is a vector containing the rows indexes")
  }
  return(list.groups)
}
