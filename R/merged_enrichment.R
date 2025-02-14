#' Merges enrichment data from a list of enrichment results of up and down regulated genes.
#'
#' @param enr_list A named list of data frames containing enrichment results.
#' @return A named list of merged enrichment data frames.
#' @export
#'
#' @importFrom dplyr select relocate contains
#'
#' @examples
#' \dontrun{
#' merged_enrichment(enr_list)
#' }
merged_enrichment <- function(enr_list){
  enr_M <- enr_list %>%
    lapply(function(x){
      x %>%
        select(- ncbiTaxonId,
               - preferredNames)
    })
  names_enrM <- names(enr_M)
  names(names_enrM) <- names_enrM

  enr_M <- lapply(names_enrM, function(x){
    colnames(enr_M[[x]])[c(3,5,6,7)] <- paste0(colnames(enr_M[[x]])[c(3,5,6,7)], "_", x)
    return(enr_M[[x]])
  })

  enr_M_all <- list()
  sq <- seq(1, length(enr_M), by = 2)
  for (i in sq){
    enr_M_all[[gsub(".up","",names(enr_M)[i])]] <- merge(enr_M[[i]], enr_M[[i+1]],
                                                         by = intersect(colnames(enr_M[[i]]), colnames(enr_M[[i+1]])),
                                                         all = TRUE) %>%
      relocate(term, description,
               contains("number_of_genes"),
               contains("p_value"),
               contains("fdr"),
               contains("input_genes"),
               .after = "category")
    ind_ig <- grep("inputGenes", colnames(enr_M_all[[gsub(".up","",names(enr_M)[i])]]))
    enr_M_all[[gsub(".up","",names(enr_M)[i])]][,ind_ig] <- lapply(enr_M_all[[gsub(".up","",names(enr_M)[i])]][,ind_ig], function(x) {
      lapply(x, function(y) ifelse(is.na(y), "", y))
    })
  }

  return(enr_M_all)
}
