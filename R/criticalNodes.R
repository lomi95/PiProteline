#' Identify specific critical nodes in a dataset
#'
#' This function identifies critical nodes that are specific to certain groups based on
#' centralities and the selected search mode. It can filter the dataset to return
#' only those critical nodes that are exclusive to a particular group or to particular centralities.
#'
#' @param x A data frame containing the data on groups and centralities.
#' @param names_of_groups A vector of group names. Each name should correspond to
#'   one or more columns in the data frame.
#' @param centralities A vector of centralities to search for. These should match
#'   the names of the centrality columns in the data frame.
#' @param searching.mode A string that specifies the search mode. Options are:
#' \describe{
#'   \item{"specific" or "s"}{ - Finds critical nodes that are present in one group but
#'       absent in all others.}
#'   \item{"centralityspecific" or "cs"}{ - Finds critical nodes that are specific to
#'       centralities within one group and absent in the centralities of other groups.}
#'  }
#' @param ignoreCase A logical that indicate whether the names_of_groups should be searched with case sensitive.
#'
#' @return A list of data frames, where each data frame contains the critical nodes specific
#'   to the corresponding group.
#'
#' @keywords hubs bottlenecks
#' @examples
#' # Example data frame
#' x <- data.frame(groupA_centrality1 = c(1, 0, 1),
#'                 groupA_centrality2 = c(1, 0, 1),
#'                 groupB_centrality1 = c(0, 1, 1),
#'                 groupB_centrality2 = c(0, 1, 0),
#'                 row.names = c("gene1", "gene2", "gene3"))
#'
#' # Define groups and centralities
#' names_of_groups <- c("groupA", "groupB")
#' centralities <- c("centrality1", "centrality2")
#'
#' # Find group-specific critical nodes
#' criticalNodes(x, names_of_groups, centralities, "specific")
#'
#' @export

criticalNodes <- function(x, names_of_groups, centralities, searching.mode, ignoreCase = T) {
  # Assign names to the groups
  names(names_of_groups) <- names_of_groups


  # Apply the filter for each group in names_of_groups
  lapply(names_of_groups, function(group) {

    if (all(!grepl(group,colnames(x),ignore.case = ignoreCase))){
      old_opt <- options()$warn
      options(warn = 1)
      warning(group, " was not found in 'colnames(x)'")
      options(warn = old_opt)
    }
    # Find columns that contain the group name and any centrality measure
    ind.group <- rowSums(sapply(centralities, function(cent) {
      col_indices <- which(grepl(group, colnames(x), ignore.case = ignoreCase) & grepl(cent, colnames(x)))
      # Rows where the group and centrality measure are both present
      rowSums(x[, col_indices, drop = FALSE], na.rm = T) > 0
    }),na.rm = T) == length(centralities)

    if (searching.mode %in% c("specific", "s")) {
      # Exclude genes that also appear in other groups
      for (other_group in names_of_groups[setdiff(names_of_groups, group)]) {
        # Further filter by excluding matches from other groups
        ind.exclude <- !(rowSums(sapply(centralities, function(cent) {
          col_indices <- which(grepl(other_group, colnames(x), ignore.case = ignoreCase) & grepl(cent, colnames(x)))
          rowSums(x[, col_indices, drop = FALSE],na.rm = T) > 0
        }), na.rm = T) == length(centralities))
        ind.group <- ind.group & ind.exclude
      }
    } else if (searching.mode %in% c("centralityspecific", "cs")) {
      # Exclude all other groups for centrality-specific filtering
      col_indices <- unlist(lapply(centralities, function(cent) {
        which(!grepl(group, colnames(x), ignore.case = ignoreCase) & grepl(cent, colnames(x)))
      }))

      ind.exclude <- rowSums(x[, col_indices, drop = FALSE], na.rm = T) == 0

      ind.group <- ind.group & ind.exclude
    }

    # Return the rows that match the group and centrality criteria
    x[ind.group,,drop = FALSE]
  })
}
