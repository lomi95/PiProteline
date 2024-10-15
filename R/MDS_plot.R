#' MDS Plot Using Random Forest Proximity
#'
#' This function generates a Multi-Dimensional Scaling (MDS) plot using the proximity matrix from a random forest classification.
#'
#' @param dataset A matrix or data frame where rows represent samples and columns represent features (e.g., proteins).
#' @param names_of_groups A character vector specifying the class names to group by, based on row names of the dataset. Ignored if `pos.vectors_groups` is provided.
#' @param ignoreCase Logical, if TRUE (default), the matching of `names_of_groups` is case insensitive.
#' @param pos.vectors_groups A list where each element contains row indices for each class group. Default is NULL, in which case `names_of_groups` is used.
#' @param n_tree Integer, the number of trees to use in the random forest. Default is 5000.
#'
#' @importFrom randomForest randomForest
#' @importFrom stats dist cmdscale
#' @importFrom ggplot2 ggplot aes geom_text theme_bw xlab ylab ggtitle
#'
#' @return A ggplot object representing the MDS plot based on (1 - random forest proximity).
#'
#' @details The MDS plot is constructed by first training a random forest model to calculate the proximity between samples. Then, Multi-Dimensional Scaling (MDS) is applied to the proximity matrix to reduce the dimensionality, and the plot shows the first two dimensions (MDS1 and MDS2) with their respective variances explained.
#'
#' @examples
#' dataset <- matrix(rnorm(100), nrow = 10)
#' rownames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2",
#'                        "control_3", "treatment_3", "control_4", "control_5",
#'                        "treatment_4", "treatment_5")
#' MDS_plot(dataset, names_of_groups = c("control", "treatment"))
#'
#' @export
MDS_plot <- function(dataset,
                     names_of_groups,
                     ignoreCase = TRUE,
                     pos.vectors_groups = NULL,
                     n_tree = 5000) {
  labels <- vector(length = nrow(dataset))

  if (!is.null(pos.vectors_groups)) {
    if (is.list(pos.vectors_groups)) {
      indxs <- table(unlist(pos.vectors_groups))
      if (max(indxs) > 1) {
        warning(paste(rownames(dataset)[which(indxs > 1)], collapse = ", "), " is/are selected for more than one group")
      }

      if (is.null(names(pos.vectors_groups))) {
        names(pos.vectors_groups) <- paste0("Group_", seq(1, length(pos.vectors_groups)))
      }
      names_of_groups <- names(pos.vectors_groups)

      for (i in names_of_groups) {
        labels[pos.vectors_groups[[i]]] <- i
      }
    } else {
      stop("'pos.vectors_groups' should be a list with column indexes for each element")
    }
  } else {
    indxs <- table(unlist(lapply(names_of_groups, function(x) {
      grep(x, rownames(dataset), ignore.case = ignoreCase)
    })))
    if (max(indxs) > 1) {
      warning(paste(rownames(dataset)[which(indxs > 1)], collapse = ", "), " is/are selected for more than one group")
    }

    for (i in names_of_groups) {
      labels[grep(i, rownames(dataset), ignore.case = TRUE)] <- i
    }
  }

  dm.lda <- data.frame(dataset, labels = as.factor(labels))
  colnames(dm.lda) <- gsub("-", "", colnames(dm.lda))

  if (sum(nchar(colnames(dm.lda)) >= 15)) {
    message(paste(paste(colnames(dataset)[nchar(colnames(dataset)) >= 15], collapse = ", "),
                  "is/are exceeding the nchar limit of 15, it/they will be removed.
                 Consider changing these names in the dataset"))
    dm.lda <- dm.lda[, nchar(colnames(dm.lda)) < 15]
  }

  rf <- randomForest(labels ~ ., dm.lda, proximity = TRUE, ntree = n_tree)
  distance.matrix <- dist(1 - rf$proximity)
  mds.stuff <- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE)

  mds.var.per <- round(mds.stuff$eig / sum(mds.stuff$eig) * 100, 1)

  mds.values <- mds.stuff$points
  mds.data <- data.frame(Sample = labels, X = mds.values[, 1], Y = mds.values[, 2], Status = dm.lda$labels)

  mds <- ggplot(data = mds.data, aes(x = X, y = Y, label = Sample)) +
    geom_text(aes(color = Status)) +
    theme_bw() +
    xlab(paste("MDS1 - ", mds.var.per[1], "%", sep = "")) +
    ylab(paste("MDS2 - ", mds.var.per[2], "%", sep = "")) +
    ggtitle("MDS plot using (1 - Random Forest Proximities)")

  return(mds)
}
