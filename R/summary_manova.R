#' Summary of MANOVA Analysis Results
#'
#' This function generates a summary of Multivariate Analysis of Variance (MANOVA) results for pairwise comparisons.
#' It extracts significant genes based on adjusted p-values, fold-change thresholds (FC), and differential average
#' expression thresholds (DAve). The function returns a combined gene matrix summarizing fold-change information across
#' comparisons and a list of up-regulated and down-regulated genes for each comparison.
#'
#' @param manova_pairwise_results A list of data frames, where each data frame contains pairwise MANOVA results.
#' Each data frame should have at least the following columns:
#' \itemize{
#'   \item \code{p.adj}: The adjusted p-values for each gene.
#'   \item \code{GeneName}: The name of the genes.
#'   \item Columns containing "FC" (fold-change values).
#'   \item Columns containing "DAve" (differential average expression values).
#' }
#' @param significance_manova Numeric. The significance threshold for adjusted p-values (\code{p.adj}). Genes with \code{p.adj}
#' less than or equal to this value are considered significant. Default is \code{0.05}.
#' @param fc_bounds Numeric vector of length 2. The thresholds for filtering genes based on fold-change values. Genes with any
#' fold-change value greater than or equal to \code{max(fc_bounds)} or less than \code{min(fc_bounds)} are retained.
#' Default is \code{c(0, 0)}.
#' @param DAve_bounds Numeric vector of length 2. The thresholds for filtering genes based on differential average expression values.
#' Genes with any DAve value greater than or equal to \code{max(DAve_bounds)} or less than \code{min(DAve_bounds)} are retained.
#' Default is \code{c(0, 0)}.
#'
#' @return A list with two components:
#' \describe{
#'   \item{\code{summary_manova}}{A data frame where rows represent unique genes (\code{GeneName}) and columns contain fold-change
#'   information across comparisons.}
#'   \item{\code{prot_manova}}{A list with two sub-components for each comparison (in comparison A vs B, up and down are referred to A):
#'   \describe{
#'     \item{\code{up}}{A vector of up-regulated genes (positive fold-change values).}
#'     \item{\code{down}}{A vector of down-regulated genes (negative fold-change values).}
#'   }}
#' }
#' @importFrom purrr reduce
#' @importFrom dplyr contains across filter select
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' quantitativeAnalysis <- quantitative_analysis(dataset, names_of_groups,
#'                                               gene_column,
#'                                               significance_manova)
#'
#' manova_pairwise_results <- quantitativeAnalysis$manova_pairw_results
#'
#' summary <- summary_manova(manova_pairwise_results,
#'                           significance_manova = 0.05,
#'                           fc_bounds = c(-1.5, 1.5),
#'                           DAve_bounds = c(-2, 2))
#'
#' # Access the summary matrix
#' summary_matrix <- summary$summary_manova
#'
#' # Access the up/down-regulated gene lists
#' gene_lists <- summary$prot_manova
#' }
summary_manova <- function(manova_pairwise_results, significance_manova = 0.05, fc_bounds = c(-0,0), DAve_bounds = c(-0,0)) {

  mpr <- lapply(manova_pairwise_results, function(x){
    x %>% dplyr::filter(.data$p.adj <= significance_manova,
                 rowSums(dplyr::across(dplyr::contains("FC"),   ~ . >= max(fc_bounds)   | . < min(fc_bounds)))   > 0,
                 rowSums(dplyr::across(dplyr::contains("DAve"), ~ . >= max(DAve_bounds) | . < min(DAve_bounds))) > 0
                 ) %>%
      dplyr::select(GeneName, dplyr::contains("FC"))
  }) %>%
    purrr::reduce(merge, by = "GeneName", all = T)


  prot_manova <- lapply(mpr[,-1, drop = F], function(x){
    list(up   = mpr$GeneName[which(x>0)],
         down = mpr$GeneName[which(x<0)])
  }) %>%
    unlist(recursive = F)
  return(list(summary_manova = mpr,
              prot_manova = prot_manova))
}
