#' Linear Discriminant Analysis
#'
#' @param dataset The dataset to analyze
#' @param names_of_groups names of groups in colnames(dataset), ignored if 'pos.vector_groups' is not NULL
#' @param gene_column position of gene column. If 0 no genes column is present
#' @param ignoreCase if TRUE the 'names_of_groups' search is not case sensitive. Default = T
#' @param significance.LDA significance threshold for features pvalues. Default = 0.05
#' @param correction.LDA correction method to adjusting pvalues. Default = "BH"
#' @param pos.vectors_groups List containing column indexes for each group to compare
#' @param MDS If TRUE computes random forest classification and Multi Dimensional Scaling. Default = T
#' @param boundFC Numeric vector. Interval out of which labels are highlithed in red and labels are shown
#'
#' @importFrom matrixStats rowMaxs
#' @importFrom matrixStats rowMins
#' @importFrom stats p.adjust
#' @importFrom stats p.adjust.methods
#' @importFrom stats manova
#' @importFrom stats summary.aov
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#'
#' @return A list with linear discriminant analysis information
#' @export
#'
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
  if (gene_column){
    suppressWarnings(dataset[,gene_column][!is.na(as.numeric(substr(dataset[,gene_column],1,1)))] <-
                       paste0("X",dataset[,gene_column][!is.na(as.numeric(substr(dataset[,gene_column],1,1)))]))
  }
  dataset.t <- transpose(dataset, gene_column)

  # modify pos.vector_groups
  if (!is.null(pos.vectors_groups)){
    if (gene_column){
      pos.vectors_groups <- lapply(pos.vectors_groups, function(p){
        p <- p - as.numeric( p > gene_column)
      })
      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs)>1){
        message("Warning: ",paste(rownames(dataset.t)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
      }
    }
  }

  labels <- vector(length=nrow(dataset.t))

  if (!is.null(pos.vectors_groups)){
    if (is.list(pos.vectors_groups)){

      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs)>1){
        message("Warning: ",paste(rownames(dataset.t)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
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
    if (max(indxs)>1){
      message("Warning: ",paste(rownames(dataset.t)[which(indxs>1)], collapse = ", "),"is/are selected for more than one group")
    }

    for ( i in names_of_groups){
      labels[grep(i,rownames(dataset.t),ignore.case = T)] <- i
    }
  }





  dataset.t <- dataset.t[!labels==F,colSums(dataset.t,na.rm = T)>0]

  # se c'è qualche labels == F deve sistemare pos.vector_groups

  if (any(labels == F)){
    if (!is.null(pos.vectors_groups)){
      pos.vectors_groups <- lapply(pos.vectors_groups, function(p){
        tabgrt <- sapply(which(labels == F), function(y) p > y)
        p <- p - rowSums(tabgrt)
      })
      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs)>1){
        message("Warning: ",paste(rownames(dataset.t)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
      }
    }
  }
  labels <- labels[!(labels==F)]
  if (sum(labels==F)==length(labels)){
    stop("No labels asssigned, names of groups are not in colnames(dataset)")
  }
  final_dm_norm_t <- data.frame(labels,dataset.t)

  #MANOVA test
  dependent_vars <- cbind(as.matrix(final_dm_norm_t[,2:ncol(final_dm_norm_t)]))

  #independent variables (conditions)
  independent_var <- as.factor(final_dm_norm_t$labels)

  #MANOVA
  #for manova model insert dependent variables before tilde and independent ones after. After comma indicate the dataframe cointaning both labels and quantitative data
  manova_model <- manova(dependent_vars ~ independent_var, data = final_dm_norm_t)

  summary <- summary.aov(manova_model)
  #it creates a dataframe with the p.adjs (the first one)
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
  prot_sig <- output_LDA[output_LDA$p.adj < significance.LDA,]

  if (nrow(prot_sig)>0){

    # We modify the GeneNames so that we have them in a suitable format
    prot_sig1 <- sapply(prot_sig$GeneName,function(x) gsub("[^a-zA-Z0-9 ]","-", x))
    colnames(dataset.t) <- gsub("[^a-zA-Z0-9 ]", "-", colnames(dataset.t)) %>%
      gsub(pattern=" ",replacement ="-")

    dataset.LDA <- data.frame(dataset.t[,prot_sig1])
    colnames(dataset.LDA) <- prot_sig1
    groups.LDA <- lapply(names_of_groups, function(x){
      df <- data.frame(dataset.LDA[grep(x,rownames(dataset.t),ignore.case = T),])
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

      #####################
      Volcanos <- lapply(seq_along(names_of_groups), function(x){
        Volc.i <- lapply(seq_along(names_of_groups)[-x],function(y){
          # print(x)
          # print(y)
          # g1.vs.g2 <- rownames(updown)[updown[,x*2-1] > 0 & updown[,y*2]   > 0]
          # g2.vs.g1 <- rownames(updown)[updown[,x*2]   > 0 & updown[,y*2-1] > 0]
          g1g2 <- output_LDA$GeneName
          g1 <- colMeans(dataset.t[grep(names_of_groups[x],rownames(dataset.t)),])
          g2 <- colMeans(dataset.t[grep(names_of_groups[y],rownames(dataset.t)),])
          df <- data.frame(log2 = log2(g1/g2),
                           pv = -log10(output_LDA$p.adj),
                           gene = names(g1))
          df$gene[df$log2 >= min(boundFC) & df$log2 <= max(boundFC)] <- ""


          V.ij <- Volcano_Plot(df,boundFC, significance.LDA, names_of_groups[x], names_of_groups[y])

          return(V.ij)

        })
        names(Volc.i) <- paste0("vs.", names_of_groups[seq_along(names_of_groups)[-x]])

        return(Volc.i)
      })

      names(Volcanos) <- names_of_groups

      if (MDS){
        mds.plot <- MDS_plot(dataset.LDA,names_of_groups,
                             pos.vectors_groups = pos.vectors_groups,
                             ignoreCase = ignoreCase)
      } else {
        mds.plot <- NULL
      }

      return(list(dataset.LDA       = dataset.LDA,
                  features_p.values = output_LDA,
                  features_updown   = updown,
                  mds.plot          = mds.plot,
                  VolcanoPlots      = Volcanos))

    } else {
      message("No significant features were found, try with a greater 'significance.LDA' threshold or another 'correction.LDA' method")
      return(list(dataset.LDA       = NULL,
                  features_p.values = output_LDA,
                  features_updown   = NULL,
                  mds.plot          = NULL,
                  VolcanoPlots      = NULL))
    }

}
