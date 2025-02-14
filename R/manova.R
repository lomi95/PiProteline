#' Multivariate Analysis of Variance (manova)
#'
#' This function performs  Multivariate Analysis of Variance on a dataset, identifying significant features across multiple groups.
#'
#' @param dataset The dataset to analyze, where rows represent features (e.g., genes, proteins) and columns represent samples.
#' @param names_of_groups A character vector specifying the names of the groups present in the column names of the dataset. Ignored if `pos_vectors_groups` is provided.
#' @param gene_column The position of the column containing gene identifiers. If set to 0, no gene column is assumed.
#' @param ignore_case Logical, if TRUE (default), the matching of `names_of_groups` is case insensitive.
#' @param correction_manova A character string specifying the p-value correction method. Default is "BH" (Benjamini-Hochberg). See `p.adjust.methods` for more options.
#' @param pos_vectors_groups A list where each element contains column indices for a group of samples. If provided, `names_of_groups` is ignored.
#' @param fc_bounds A numeric vector specifying the bounds for highlighting fold changes in Volcano plots. Default is c(0, 0).
#'
#' @importFrom matrixStats rowMaxs
#' @importFrom matrixStats rowMins
#' @importFrom stats p.adjust
#' @importFrom stats manova
#' @importFrom stats summary.aov
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of
#'
#' @return A data.frame with p-values, adjusted p-values, Fold changes and DAves for the features.
#'
#' @examples
#' dataset <- matrix(rnorm(100), nrow = 20)
#' colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3")
#' rownames(dataset) <- paste("gene", 1:20, sep = "_")
#' manova(dataset, names_of_groups = c("control", "treatment"), gene_column = 0)
#'
#' @export

manova <- function(dataset,
                names_of_groups,
                gene_column,
                ignore_case = T,
                correction_manova = "BH",
                pos_vectors_groups = NULL,
                fc_bounds = c(-0,0)){

  if (!is.null(correction_manova)){
    correction_manova <- match.arg(correction_manova,choices = p.adjust.methods)
  }
  GC <- 0

  if (is.character(gene_column)){
    GC <- 1
    suppressWarnings(dataset[,gene_column][!is.na(as.numeric(substr(dataset[,gene_column],1,1)))] <-
                       paste0("X",dataset[,gene_column][!is.na(as.numeric(substr(dataset[,gene_column],1,1)))]))
  } else if (gene_column){
    GC <- 1
    suppressWarnings(dataset[,gene_column][!is.na(as.numeric(substr(dataset[,gene_column],1,1)))] <-
                       paste0("X",dataset[,gene_column][!is.na(as.numeric(substr(dataset[,gene_column],1,1)))]))
  }
  dataset.t <- PiProteline::transpose(dataset, gene_column)

  if (!is.null(pos_vectors_groups)){
    if (max(unlist(pos_vectors_groups)> (ncol(dataset) - GC))){
      stop("Attempt to select index of columns greater than number of columnns in dataset")
    }

    if (gene_column){
      pos_vectors_groups <- lapply(pos_vectors_groups, function(p){
        p <- p - as.numeric( p > gene_column)
      })
      indxs <- table(unlist(pos_vectors_groups))
      if (max(indxs)>1){
        warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),
                " is/are selected for more than one group")
      }
    }
  }

  labels <- vector(length=nrow(dataset.t))

  if (!is.null(pos_vectors_groups)){
    if (is.list(pos_vectors_groups)){

      indxs <- table(unlist(pos_vectors_groups))
      if (max(indxs)>1){
        warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),
                " is/are selected for more than one group")
      }

      if (is.null(names(pos_vectors_groups))){
        names(pos_vectors_groups) <- paste0("Group_",seq(1,length(pos_vectors_groups)))
      }
      names_of_groups <- names(pos_vectors_groups)

      for (i in names_of_groups){
        labels[pos_vectors_groups[[i]]] <- i
      }



    } else {
      stop("'pos_vectors_groups' should be a list with column indexes for each element")
    }
  } else {
    names(names_of_groups) <- names_of_groups

    indxs <- table(unlist(lapply(names_of_groups, function(x) {
      grep(x,rownames(dataset.t), ignore.case = ignore_case)
    })))
    if (!length(indxs)){
      stop("`names_of_groups` were not found in dataset colnames")
    }
    if (max(indxs)>1){
      warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),
              "is/are selected for more than one group")
    }

    for ( i in names_of_groups){
      labels[grep(i,rownames(dataset.t),ignore.case = ignore_case)] <- i
    }
  }

  dataset.t <- dataset.t[!labels==F,, drop = F]

  if (any(labels == F)){
    if (!is.null(pos_vectors_groups)){
      pos_vectors_groups <- lapply(pos_vectors_groups, function(p){
        tabgrt <- sapply(which(labels == F), function(y) p > y)
        p <- p - rowSums(tabgrt)
      })
      indxs <- table(unlist(pos_vectors_groups))
      if (max(indxs)>1){
        warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
      }
    }
  }
  labels <- labels[labels!=F]

  final_dm_norm_t <- data.frame(labels,dataset.t, check.names = F)

  #MANOVA test
  dependent_vars <- cbind(as.matrix(final_dm_norm_t[,2:ncol(final_dm_norm_t)]))
  independent_var <- as.factor(final_dm_norm_t$labels)

  #MANOVA
  #for manova model insert dependent variables before tilde and independent ones after.
  #After comma indicate the dataframe cointaning both labels and quantitative data
  manova_model <- stats::manova(dependent_vars ~ independent_var, data = final_dm_norm_t)

  summary <- summary.aov(manova_model)
  #it creates a dataframe with the p.adjs
  output_manova <- data.frame(GeneName = gsub(" Response ","",names(summary)),
                           p.value  = sapply(summary, function(x) x$`Pr(>F)`[1]))
  rownames(output_manova) <- output_manova$GeneName

  if (is.null(correction_manova)){
    output_manova$p.adj <- output_manova$p.value
  } else {
    output_manova$p.adj <- p.adjust(output_manova$p.value, correction_manova)
  }
  output_manova <- output_manova[!is.na(output_manova$p.adj),]

  if (gene_column != 0 | is.null(gene_column)){
    rownames(dataset) <- dataset[,gene_column]
  }
  data_grouped_full <- group_listing(dataset, names_of_groups, freq = 0)
  DAve_index <- DAve(data_grouped_full)
  colnames(DAve_index)[-1] <- paste0("DAve_", colnames(DAve_index)[-1])

  Fold_Change <- FC(data_grouped_full)
  colnames(Fold_Change)[-1] <- paste0("FC_", colnames(Fold_Change)[-1])

  manovaAndIndexes <- merge(output_manova, DAve_index, by = "GeneName") %>%
    merge(Fold_Change, by = "GeneName")

  return(manovaAndIndexes)

}
