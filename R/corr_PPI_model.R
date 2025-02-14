#' Correlation and PPI Network Model Construction
#'
#' This function constructs a network model based on correlations and protein-protein interactions (PPI).
#' It can compute correlation matrices for the given dataset, integrate them with an existing interactome
#' network, and optionally transform non-significant correlations into weights.
#'
#' @param dataset A matrix or data.frame containing the dataset to build network models.
#' @param genes_id A character vector of gene identifiers to build the interactome. This is ignored if
#'   \code{g_interactome} is provided.
#' @param corr_mat A correlation matrix. If provided, the function will use it instead of computing a new one. Default is \code{NULL}.
#' @param corr_test A character string specifying the correlation test to use. Options are \code{"spearman"} or \code{"pearson"}.
#'   Default is \code{"spearman"}.
#' @param significance_corr A numeric value indicating the significance threshold for correlations. Default is \code{0.05}.
#' @param correction_corr A character string specifying the p-value adjustment method for multiple testing corrections.
#'   Default is \code{"BH"} (Benjamini-Hochberg).
#' @param g_interactome An \code{igraph} object representing the interactome. If \code{NULL}, the interactome is built
#'   using \code{genes_id}. Default is \code{NULL}.
#' @param score_threshold A numeric vector specifying thresholds for filtering the interactome.
#'   It should contain two elements: \code{escore} and \code{dscore}. Default is \code{c(escore = 0.15, dscore = 0.35)}.
#' @param tax_ID An integer specifying the taxonomy ID of the species under consideration. The default is \code{9606} (human).
#' @param na_as_zero A logical value. If \code{TRUE}, all \code{NA} values in the dataset will be converted to zeros. Default is \code{FALSE}.
#' @param zero_as_na A logical value. If \code{TRUE}, all zeros in the dataset will be converted to \code{NA}. Default is \code{TRUE}.
#' @param compute_weights A logical value. If \code{TRUE}, non-significant correlations will be transformed using a weighting scheme.
#'   Default is \code{TRUE}.
#'
#' @importFrom rbioapi rba_string_map_ids
#' @importFrom rbioapi rba_string_interactions_network
#' @importFrom igraph graph_from_edgelist
#' @importFrom igraph intersection
#' @importFrom igraph V
#' @importFrom igraph edge.attributes
#' @importFrom igraph edge.attributes<-
#' @importFrom igraph as_edgelist
#' @importFrom stats p.adjust
#' @importFrom Hmisc rcorr
#'
#' @return An \code{igraph} object representing the combined correlation and PPI network model.
#'
#' @details
#' The function constructs a network model by first creating a correlation matrix from the given dataset.
#' If \code{g_interactome} is not provided, the function will build an interactome based on the specified
#' \code{genes_id} using the STRING database through \code{rbioapi}. The correlation matrix is then integrated
#' with the interactome to form a combined network model. Optionally, non-significant correlations can be
#' transformed into weights using the \code{cor2W_transform} function.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' dataset <- data.frame(
#'  Gene1 = c(0.5, NA, 0.3, 0.6, 0.9),
#'  Gene2 = c(0.4, 0.6, 0, 0.8, 0.4),
#'  Gene3 = c(NA, 0.1, 0.2, 0.6, 0.9),
#'  Gene4 = c(0.7, 0.1, 0.2, 0.4, 0.1),
#'  row.names = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5")
#' )
#'
#' # List of genes
#' genes_id <- c("Gene1", "Gene2", "Gene3", "Gene4")
#'
#' # Build the network model
#' network_model <- corr_PPI_model(
#'   dataset = dataset,
#'   genes_id = genes_id,
#'   corr_test = "spearman",
#'   significance_corr = 0.05,
#'   tax_ID = 9606,
#'   na_as_zero = TRUE,
#'   zero_as_na = FALSE
#' )
#' }
#'
#' @seealso \code{\link[rbioapi]{rba_string_map_ids}}, \code{\link[rbioapi]{rba_string_interactions_network}}, \code{\link[igraph]{graph_from_edgelist}}
#'
#' @export

corr_PPI_model <- function(dataset,
                           genes_id = NULL,
                           corr_mat = NULL,
                           corr_test = "spearman",
                           significance_corr = 0.05,
                           correction_corr = "BH",
                           g_interactome = NULL,
                           score_threshold = c(escore = 0.15, dscore = 0.35),
                           tax_ID = 9606,
                           na_as_zero = F, zero_as_na = T,
                           compute_weights = T){
  if (na_as_zero & zero_as_na){
    stop("'na_as_zero' and 'zero_as_na' are both TRUE, set at least one of them as FALSE")
  }

  if (is.null(g_interactome)){
    # mapIds <- rba_string_map_ids(genes_id,
    #                              species = tax_ID)
    # indMatched    <- match(genes_id,map_ids$queryItem)
    # genesNotFound <- setdiff(genes_id,map_ids$queryItem)
    rba_string_interactions_network(genes_id[1],tax_ID, verbose = F) # Ping
    interactome.0 <- rba_string_interactions_network(genes_id,
                                                     species = tax_ID,
                                                     required_score = 0)
    interactome <- filter_interactome(interactome.0,scores_threshold = score_threshold)
    g_interactome <- graph_from_edgelist(as.matrix(interactome[,3:4]),directed = F)


  }


  if (is.null(corr_mat)){
    if (zero_as_na){
      dataset[dataset==0] <- NA
    } else if (na_as_zero){
      dataset[is.na(dataset)] <- 0
    }
    corr_mat <- tryCatch(Hmisc::rcorr(as.matrix(dataset), type = corr_test), error = function(e){
      if (e$message == "must have >4 observations"){
        warning("To compute correlation datasets must have >4 observations")
      } else {
        warning(paste0(e))
      }
      list(r = matrix(0, nrow = ncol(dataset), ncol = ncol(dataset),
                      dimnames = list(colnames(dataset), colnames(dataset))),
           P = matrix(1, nrow = ncol(dataset), ncol = ncol(dataset),
                      dimnames = list(colnames(dataset), colnames(dataset))),
           n = matrix(0, nrow = ncol(dataset), ncol = ncol(dataset),
                      dimnames = list(colnames(dataset), colnames(dataset))))
    })
  }

  flattCorr <- flattenCorrMatrix(corr_mat$r,corr_mat$P,corr_mat$n)
  flattCorr$cor_features <- paste(flattCorr$row,"and",flattCorr$column)
  flattCorr$cor[is.na(flattCorr$cor)] <- 0
  flattCorr$p[is.na(flattCorr$p)] <- 1


  corrGraph <- graph_from_edgelist(as.matrix(flattCorr[,1:2]), directed = F)

  corr.PPI_0 <- intersection(corrGraph, g_interactome)
  corr.PPI_0.el <- as_edgelist(corr.PPI_0)
  corr.PPI_0.features <- paste(corr.PPI_0.el[,1],"and",corr.PPI_0.el[,2])
  corr.PPI_0.features.rev <- paste(corr.PPI_0.el[,2],"and",corr.PPI_0.el[,1])


  flatt_Corr.PPI <- flattCorr[(flattCorr$cor_features %in% corr.PPI_0.features) |
                                (flattCorr$cor_features %in% corr.PPI_0.features.rev),]

  if (is.null(correction_corr)){
    flatt_Corr.PPI$p.adj <- flatt_Corr.PPI$p
  } else {
    flatt_Corr.PPI$p.adj <- p.adjust(flatt_Corr.PPI$p, correction_corr)
  }
  ind_sig <- flatt_Corr.PPI$p.adj < significance_corr

  if (compute_weights){
    flatt_Corr.PPI <- cor2W_transform(flatt_Corr.PPI,significance_corr)
  } else {
    flatt_Corr.PPI$weights <- flatt_Corr.PPI$cor
    flatt_Corr.PPI$weights[!ind_sig] <- 0
  }

  g.CorrPPI <- graph_from_edgelist(as.matrix(flatt_Corr.PPI[,1:2]), directed = F)
  edge.attributes(g.CorrPPI)$weights <- flatt_Corr.PPI$weights

  return(g.CorrPPI)

}
