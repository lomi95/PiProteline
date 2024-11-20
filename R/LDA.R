#' Linear Discriminant Analysis (LDA)
#'
#' This function performs linear discriminant analysis on a dataset, identifying significant features across multiple groups. It can also compute random forest classification and Multi-Dimensional Scaling (MDS) if specified.
#'
#' @param dataset The dataset to analyze, where rows represent features (e.g., genes, proteins) and columns represent samples.
#' @param names_of_groups A character vector specifying the names of the groups present in the column names of the dataset. Ignored if `pos.vectors_groups` is provided.
#' @param gene_column The position of the column containing gene identifiers. If set to 0, no gene column is assumed.
#' @param ignoreCase Logical, if TRUE (default), the matching of `names_of_groups` is case insensitive.
#' @param significance.LDA Numeric, the significance threshold for feature p-values. Default is 0.05.
#' @param correction.LDA A character string specifying the p-value correction method. Default is "BH" (Benjamini-Hochberg). See `p.adjust.methods` for more options.
#' @param pos.vectors_groups A list where each element contains column indices for a group of samples. If provided, `names_of_groups` is ignored.
#' @param boundFC A numeric vector specifying the bounds for highlighting fold changes in Volcano plots. Default is c(-2, 2).
#' @param MDS Logical, if TRUE (default), computes random forest classification and Multi-Dimensional Scaling (MDS).
#'
#' @importFrom matrixStats rowMaxs
#' @importFrom matrixStats rowMins
#' @importFrom stats p.adjust
#' @importFrom stats manova
#' @importFrom stats summary.aov
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of
#'
#' @return A list containing:
#' \item{dataset.LDA}{The filtered dataset with significant features.}
#' \item{features_p.values}{A dataframe with p-values and adjusted p-values for the features.}
#' \item{features_updown}{A matrix indicating which features are up- or down-regulated in each group.}
#'
#' @examples
#' dataset <- matrix(rnorm(100), nrow = 20)
#' colnames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3")
#' rownames(dataset) <- paste("gene", 1:20, sep = "_")
#' LDA(dataset, names_of_groups = c("control", "treatment"), gene_column = 0)
#'
#' @export

LDA <- function(dataset,
                names_of_groups,
                gene_column,
                ignoreCase = T,
                significance.LDA = 0.05,
                correction.LDA = "BH",
                pos.vectors_groups = NULL,
                boundFC = c(-2,2),
                MDS = T){

  if (!is.null(correction.LDA)){
    correction.LDA <- match.arg(correction.LDA,choices = p.adjust.methods)
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
  dataset.t <- transpose(dataset, gene_column)

  if (!is.null(pos.vectors_groups)){
    if (max(unlist(pos.vectors_groups)> (ncol(dataset) - GC))){
      stop("Attempt to select index of columns greater than number of columnns in dataset")
    }

    if (gene_column){
      pos.vectors_groups <- lapply(pos.vectors_groups, function(p){
        p <- p - as.numeric( p > gene_column)
      })
      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs)>1){
        warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),
                " is/are selected for more than one group")
      }
    }
  }

  labels <- vector(length=nrow(dataset.t))

  if (!is.null(pos.vectors_groups)){
    if (is.list(pos.vectors_groups)){

      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs)>1){
        warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),
                " is/are selected for more than one group")
      }

      if (is.null(names(pos.vectors_groups))){
        names(pos.vectors_groups) <- paste0("Group_",seq(1,length(pos.vectors_groups)))
      }
      names_of_groups <- names(pos.vectors_groups)

      for (i in names_of_groups){
        labels[pos.vectors_groups[[i]]] <- i
      }



    } else {
      stop("'pos.vectors_groups' should be a list with column indexes for each element")
    }
  } else {
    names(names_of_groups) <- names_of_groups

    indxs <- table(unlist(lapply(names_of_groups, function(x) {
      grep(x,rownames(dataset.t), ignore.case = ignoreCase)
    })))
    if (!length(indxs)){
      stop("`names_of_groups` were not found in dataset colnames")
    }
    if (max(indxs)>1){
      warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),
              "is/are selected for more than one group")
    }

    for ( i in names_of_groups){
      labels[grep(i,rownames(dataset.t),ignore.case = T)] <- i
    }
  }





  dataset.t <- dataset.t[!labels==F,, drop = F]

  if (any(labels == F)){
    if (!is.null(pos.vectors_groups)){
      pos.vectors_groups <- lapply(pos.vectors_groups, function(p){
        tabgrt <- sapply(which(labels == F), function(y) p > y)
        p <- p - rowSums(tabgrt)
      })
      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs)>1){
        warning(paste(rownames(dataset.t)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
      }
    }
  }
  labels <- labels[labels!=F]

  final_dm_norm_t <- data.frame(labels,dataset.t)

  #MANOVA test
  dependent_vars <- cbind(as.matrix(final_dm_norm_t[,2:ncol(final_dm_norm_t)]))
  independent_var <- as.factor(final_dm_norm_t$labels)

  #MANOVA
  #for manova model insert dependent variables before tilde and independent ones after.
  #After comma indicate the dataframe cointaning both labels and quantitative data
  manova_model <- manova(dependent_vars ~ independent_var, data = final_dm_norm_t)

  summary <- summary.aov(manova_model)
  #it creates a dataframe with the p.adjs
  output_LDA <- data.frame(GeneName = gsub(" Response ","",names(summary)),
                           p.value  = sapply(summary, function(x) x$`Pr(>F)`[1]))
  rownames(output_LDA) <- output_LDA$GeneName
  if (is.null(correction.LDA)){
    output_LDA$p.adj <- output_LDA$p.value
  } else {
    output_LDA$p.adj <- p.adjust(output_LDA$p.value, correction.LDA)
  }
  output_LDA <- output_LDA[!is.na(output_LDA$p.adj),]

  # We select the significant features
  prot_sig <- output_LDA[output_LDA$p.adj < significance.LDA,,drop = F]

  if (nrow(prot_sig)>0){

    # We modify the GeneNames so that we have them in a suitable format
    prot_sig1 <- sapply(prot_sig$GeneName,function(x) gsub("[^a-zA-Z0-9 ]","-", x))
    colnames(dataset.t) <- gsub("[^a-zA-Z0-9 ]", "-", colnames(dataset.t)) %>%
      gsub(pattern=" ",replacement ="-")

    dataset.LDA <- data.frame(dataset.t[,prot_sig1, drop = F])
    groups.LDA <- lapply(names_of_groups, function(x){
      df <- data.frame(dataset.LDA[grep(x,rownames(dataset.t),ignore.case = T),, drop = F])
      colnames(df) <- prot_sig1
      return(df)
    })

    if (ncol(dataset.LDA==1)){
      meancol <- matrix(sapply(groups.LDA, colMeans),
                        nrow = ncol(groups.LDA[[1]]), ncol = length(groups.LDA),
                        dimnames = list(colnames(groups.LDA[[1]]), names(groups.LDA)))
    } else {
      meancol <- sapply(groups.LDA, colMeans)
    }
    updown <- matrix(0,nrow = nrow(meancol),ncol = ncol(meancol)*2,
                     dimnames = list(rownames(meancol),
                                     unlist(lapply(colnames(meancol),
                                                   paste0,c("_up","_down")))))
    for (i in 1:ncol(meancol)){
      updown[,2*i-1] <- ifelse(meancol[,i]==rowMaxs(meancol),1,0)
      updown[,2*i]   <- ifelse(meancol[,i]==rowMins(meancol),1,0)
    }


    return(list(dataset.LDA       = dataset.LDA,
                features_p.values = output_LDA,
                features_updown   = updown))

  } else {
    message("No significant features were found, try with a greater 'significance.LDA' threshold or another 'correction.LDA' method")
    return(list(dataset.LDA       = NULL,
                features_p.values = output_LDA,
                features_updown   = NULL))
  }

}

