#' Generate Volcano Plots for Group Comparisons
#'
#' This function generates volcano plots for pairwise comparisons between groups
#' in a given dataset. For each comparison, it calculates fold change (log2) and
#' significance (-log10 p-values) and filters genes based on given thresholds.
#'
#' @param dataset.t A numeric matrix or data frame where rows represent features (e.g., genes)
#' and columns represent samples. The last row should contain p-values.
#' @param names_of_groups A character vector specifying the names of the groups to compare.
#' @param significance A numeric value specifying the significance threshold (-log10 p-value).
#' @param boundFC A numeric vector of length 2 defining the fold-change bounds (log2 scale).
#' @param ignoreCase Logical; if TRUE (default), group names are matched ignoring case.
#'
#' @return A nested list of volcano plots for each group. The outer list contains an
#' element for each group, and the inner list contains pairwise comparisons between
#' the selected group and all others.
#' @export
#'
#' @examples
#' \dontrun{
#' dataset.t <- matrix(runif(200), nrow = 10)
#' rownames(dataset.t) <- paste0("Gene_", 1:9)
#' dataset.t <- rbind(dataset.t, runif(20, min = 0, max = 0.05))
#' colnames(dataset.t) <- c("Group1", "Group2", "Group3")
#'
#' volcano_groups(dataset.t, names_of_groups = c("Group1", "Group2", "Group3"),
#'                significance = 0.05, boundFC = c(-1, 1))
#' }
volcano_groups <- function(dataset.t, names_of_groups, significance = 0.05, boundFC = c(-2,2), ignoreCase = T){

  Volcanos <- lapply(seq_along(names_of_groups), function(x){
    Volc.i <- lapply(seq_along(names_of_groups)[-x],function(y){

      g1g2 <- colnames(dataset.t)
      g1 <- colMeans(dataset.t[grep(names_of_groups[x],rownames(dataset.t),ignore.case = ignoreCase),, drop = F])
      g2 <- colMeans(dataset.t[grep(names_of_groups[y],rownames(dataset.t),ignore.case = ignoreCase),, drop = F])
      df <- data.frame(log2 = log2(g1/g2)[g1g2],
                       pv   = as.vector(t(-log10(dataset.t[nrow(dataset.t),]))),
                       gene = g1g2)
      df$gene[df$log2 >= min(boundFC) & df$log2 <= max(boundFC)] <- ""


      V.ij <- volcano_plot(df,boundFC, significance, names_of_groups[x], names_of_groups[y])

      return(V.ij)

    })
    names(Volc.i) <- paste0("vs.", names_of_groups[seq_along(names_of_groups)[-x]])

    return(Volc.i)
  })

  names(Volcanos) <- names_of_groups

  return(Volcanos)
}

