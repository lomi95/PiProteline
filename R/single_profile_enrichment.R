#' Single Profile Enrichment Analysis
#'
#' This function performs a profile enrichment analysis on a specified dataset by
#' identifying enriched terms for each group of proteins. The function supports both
#' parallel and sequential computation modes.
#'
#' @param dataset A data frame where rows represent genes and columns contain
#' information across groups. Non-numeric columns are ignored, and all values
#' greater than 0 are considered as present.
#' @param names_of_groups A character vector specifying the names of the groups
#' in `dataset` for profile enrichment.
#' @param gene_column An integer or character specifying the column in `dataset`
#' that contains gene identifiers. Default is the first column.
#' @param tax_ID Numeric species taxonomic ID used in enrichment analysis.
#' @param categories A character vector of categories for enrichment filtering.
#' Default categories include "Process", "Function", "Component", "RCTM", and "WikiPathways".
#' @param parallel Logical; if TRUE, the function will execute in parallel mode.
#' Default is TRUE.
#' @param num_cores Integer specifying the number of cores to use for parallel
#' @param ... Other arguments to pass on internal functions
#' processing. Default is one less than the total available cores.
#'
#' @return A data frame containing enrichment results per category and term, with
#' additional columns for computed metrics such as means, DAve, and manova-adjusted
#' p-values.
#'
#' @details This function first identifies proteins in each group and applies
#' enrichment analysis. Enrichment results are then filtered by selected categories and
#' metrics are calculated.
#'
#' @importFrom dplyr pull filter
#' @importFrom parallel stopCluster makeCluster
#' @examples
#' \dontrun{
#' dataset <- data.frame(geneID = c("gene1", "gene2", "gene3"),
#'                       group1 = c(1, 0, 1),
#'                       group2 = c(0, 1, 0))
#' single_profile_enrichment(dataset, names_of_groups = c("group1", "group2"),
#'                           tax_ID = 9606)
#' }
#'
#' @export


single_profile_enrichment <- function(dataset, names_of_groups, gene_column = 1,
                                      tax_ID,categories = c("Process", "Function", "Component", "RCTM", "WikiPathways"),
                                      parallel = T, num_cores = parallel::detectCores()-1, ...){
  args_list <- list(...)[[1]]
  if (is.character(gene_column)){
    if (any(gene_column == colnames(dataset))){
      gene_column <- which(gene_column == colnames(dataset))
    } else {
      stop("Give a valid 'gene_column' value")
    }

  }

  identified_prots <- lapply(dataset[,-gene_column], function(x) dataset[x>0,gene_column])
  no_prots <- sapply(identified_prots, length) == 0
  for (i in seq_along(no_prots)){
    if (no_prots[i]) identified_prots[[i]] <- c("RPL24","RPL26","RPL29","RPL34","RPS15","RPS24")
  }

  if (parallel){
    on.exit(parallel::stopCluster(cl))
    num_cores <- floor(min(num_cores,length(identified_prots)/4))
    cl <- parallel::makeCluster(num_cores)

    singleEnrichments <- parallel::parLapply(cl = cl, X = identified_prots,
                                             rbioapi::rba_string_enrichment,
                                             species = tax_ID, split_df = F,
                                             verbose = F)


    if (any(sapply(singleEnrichments, class) == "character")){
      ind_err <- which(sapply(singleEnrichments, class) == "character")
      singleEnrichments[ind_err] <- lapply(identified_prots[ind_err],
               rbioapi::rba_string_enrichment,
               species = tax_ID, split_df = F,
               verbose = F)
    }
  } else {
    singleEnrichments <- lapply(identified_prots,
                                rbioapi::rba_string_enrichment,
                                species = tax_ID, split_df = F,
                                verbose = F)
  }
  for (i in seq_along(no_prots)){
    if (no_prots[i]) singleEnrichments[[i]] <- singleEnrichments[[i]][0,]
  }


  singleEnrichments2 <- lapply(names(singleEnrichments),  function(x){

    colnames(singleEnrichments[[x]])[3] <- x
    return(singleEnrichments[[x]][c(1,2,3,10)])
  })

  enrTable <- purrr::reduce(singleEnrichments2, merge, by = c("category","term","description"), all = T) %>%
    filter(category %in% categories)
  enrTable[is.na(enrTable)] <- 0

  args_manova <- args_list[intersect(names(args_list), names(formals(manova)))]
  args_manova <- args_manova[names(args_manova) != "correction_manova"]

  ldaEnrTable <- manova(dataset = enrTable[,-c(1,3)],
                        names_of_groups,
                        gene_column = 1,
                        correction_manova = NULL,
                        )
  ldaEnrTable1 <- enrTable %>%
    filter(term %in% (ldaEnrTable %>%
                        filter(p.adj <= 0.05) %>%
                        pull(GeneName))) %>%
    select(-category, -description)

  if (nrow(ldaEnrTable1)){
    rownames(ldaEnrTable1) <- ldaEnrTable1$term
    groupsEnrTable <- group_listing(ldaEnrTable1,names_of_groups)
    meanEnrTable <- sapply(groupsEnrTable, rowMeans)
    colnames(meanEnrTable) <- paste0("Mean_",colnames(meanEnrTable))
    daveEnrTable <- DAve(groupsEnrTable)
    colnames(daveEnrTable) <- paste0("DAve_",colnames(daveEnrTable))

    ldaEnrTable2 <- data.frame(enrTable[match(rownames(ldaEnrTable1),enrTable[,2]),1:3],
                               pvalue_manova = ldaEnrTable %>%
                                 filter(p.adj <= 0.05) %>%
                                 pull(p.adj),
                               daveEnrTable %>%  select(-DAve_GeneName),
                               meanEnrTable,
                               ldaEnrTable1 %>% select(-term)
    )
    return(ldaEnrTable2)
  } else {
    return(ldaEnrTable1)
  }
}

