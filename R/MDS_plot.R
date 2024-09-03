#' MDS plot
#'
#' @param dataset The dataset we want to classify, Samples x Proteins
#' @param names_of_groups The names of the classes in rownames(dataset), ignored if 'pos.vectors_groups' is not NULL
#' @param ignoreCase if TRUE the 'names_of_groups' search is not case sensitive. Default = T
#' @param pos.vectors_groups A list object with in each element there are row indexes for classes. Default NULL
#' @param n_tree number of random forest trees
#'
#' @importFrom randomForest randomForest
#' @importFrom stats dist
#' @importFrom stats cmdscale
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#'
#' @return A MDS plot
#' @export
#'
MDS_plot <- function(dataset,
                     names_of_groups,
                     ignoreCase = T,
                     pos.vectors_groups = NULL,
                     n_tree = 5000){
  labels <- vector(length=nrow(dataset))

  if (!is.null(pos.vectors_groups)){
    if (is.list(pos.vectors_groups)){

      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs)>1){
        message("Warning: ",paste(rownames(dataset)[which(indxs>1)], collapse = ", ")," is/are selected for more than one group")
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

    indxs <- table(unlist(lapply(names_of_groups, function(x) {
      grep(x,rownames(dataset), ignore.case = ignoreCase)
    })))
    if (max(indxs)>1){
      message("Warning: ",paste(rownames(dataset)[which(indxs>1)], collapse = ", "),"is/are selected for more than one group")
    }

    for ( i in names_of_groups){
      labels[grep(i,rownames(dataset),ignore.case = T)] <- i
    }
  }

  dm.lda <- data.frame(dataset,
                       labels = as.factor(labels))
  colnames(dm.lda) <- gsub("-","",colnames(dm.lda))

  if (sum(nchar(colnames(dm.lda)) >= 15)){
    message(paste(colnames(dm.lda)[nchar(colnames(dm.lda)) >= 15], collapse = ", "),
            " is/are exceeding the nchar limit of 15, it/they will be removed.
            Consider to change these names in the dataset")
    dm.lda <- dm.lda[,nchar(colnames(dm.lda)) < 15]
  }

  rf <- randomForest(labels ~ . , dm.lda, proximity = T, ntree = n_tree)
  distance.matrix <- dist(1- rf$proximity)
  mds.stuff <- cmdscale(distance.matrix, eig = T, x.ret = T)

  mds.var.per  <- round(mds.stuff$eig/sum(mds.stuff$eig)*100,1 )

  mds.values <- mds.stuff$points
  mds.data <- data.frame(Sample = labels,
                         X = mds.values[,1],
                         Y = mds.values[,2],
                         Status = dm.lda$labels)
  mds <- ggplot(data = mds.data, aes(x=X, y=Y, label = Sample)) +
    geom_text(aes(color=Status)) +
    theme_bw() +
    xlab(paste("MDS1 - ", mds.var.per[1], "%", sep = "")) +
    ylab(paste("MDS2 - ", mds.var.per[2], "%", sep = "")) +
    ggtitle("MDS plot using (1 - Random Forest Proximities)")
  return(mds)
}
