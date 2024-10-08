#' Title
#'
#' @param dataset dataset to build network models
#' @param genes_id vector of genes to build interactome, ignored if 'interactome' is given
#' @param corr.test "spearman" or "pearson". Default = "spearman"
#' @param signifCorr significance threshold. Default = 0.05
#' @param correctionCorr correction method for correlation pvalues. Default = "BH"
#' @param interactome.graph igraph interactome. If NULL is built on 'genes_id'. default = NULL
#' @param tax_ID taxonomy ID of the spices into consideration. default human = 9606
#' @param NAasZero if TRUE all NAs will be converted in zeros, default F
#'      if both NAasZero and ZeroasNA are TRUE, it will give an error
#' @param ZeroasNA if TRUE all zeros will be converted in NAs, default T
#' @param compute_weights if TRUE non significant correlations will be transformed.
#'     Default = T
#' @param Corr Correlation if given
#' @param score_threshold thresholds for interactome. default...
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
#' @return A list with PPI/Co-expression network model for each condition
#' @export
#'
Corr_PPI.model <- function(dataset,
                           genes_id = NULL,
                           Corr = NULL,
                           corr.test = "spearman",
                           signifCorr = 0.05,
                           correctionCorr = "BH",
                           interactome.graph = NULL,
                           score_threshold = c(escore = 0.15, dscore = 0.35),
                           tax_ID = 9606,
                           NAasZero = F, ZeroasNA = T,
                           compute_weights = T){
  if (NAasZero & ZeroasNA){
    stop("'NAasZero' and 'ZeroasNA' are both TRUE, set at least one of them as FALSE")
  }

  if (is.null(interactome.graph)){
    # mapIds <- rba_string_map_ids(genes_id,
    #                              species = tax_ID)
    # indMatched    <- match(genes_id,map_ids$queryItem)
    # genesNotFound <- setdiff(genes_id,map_ids$queryItem)
    rba_string_interactions_network(genes_id[1],tax_ID, verbose = F) # Ping
    interactome.0 <- rba_string_interactions_network(genes_id,
                                                     species = tax_ID,
                                                     required_score = 0)
    interactome <- filter_interactome(interactome.0,scores_threshold = score_threshold)
    interactome.graph <- graph_from_edgelist(as.matrix(interactome[,3:4]),directed = F)


  }


  if (is.null(Corr)){
    if (ZeroasNA){
      dataset[dataset==0] <- NA
    } else if (NAasZero){
      dataset[is.na(dataset)] <- 0
    }
    Corr <- rcorr(as.matrix(dataset), type = corr.test)
  }

  flattCorr <- flattenCorrMatrix(Corr$r,Corr$P,Corr$n)
  flattCorr$cor_features <- paste(flattCorr$row,"and",flattCorr$column)
  flattCorr$cor[is.na(flattCorr$cor)] <- 0
  flattCorr$p[is.na(flattCorr$p)] <- 1


  corrGraph <- graph_from_edgelist(as.matrix(flattCorr[,1:2]), directed = F)

  corr.PPI_0 <- intersection(corrGraph, interactome.graph)
  corr.PPI_0.el <- as_edgelist(corr.PPI_0)
  corr.PPI_0.features <- paste(corr.PPI_0.el[,1],"and",corr.PPI_0.el[,2])
  corr.PPI_0.features.rev <- paste(corr.PPI_0.el[,2],"and",corr.PPI_0.el[,1])


  flatt_Corr.PPI <- flattCorr[(flattCorr$cor_features %in% corr.PPI_0.features) |
                                (flattCorr$cor_features %in% corr.PPI_0.features.rev),]

  if (is.null(correctionCorr)){
    flatt_Corr.PPI$p.adj <- flatt_Corr.PPI$p
  } else {
    flatt_Corr.PPI$p.adj <- p.adjust(flatt_Corr.PPI$p, correctionCorr)
  }
  ind_sig <- flatt_Corr.PPI$p.adj < signifCorr

  if (compute_weights){
    flatt_Corr.PPI <- cor2W_transform(flatt_Corr.PPI,signifCorr)
  } else {
    flatt_Corr.PPI$weights <- flatt_Corr.PPI$cor
    flatt_Corr.PPI$weights[!ind_sig] <- 0
  }

  g.CorrPPI <- graph_from_edgelist(as.matrix(flatt_Corr.PPI[,1:2]), directed = F)
  edge.attributes(g.CorrPPI)$weights <- flatt_Corr.PPI$weights

  return(g.CorrPPI)

}
