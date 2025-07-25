#' Filter a Data Frame by P-value or Adjusted P-value (FDR)
#'
#'
#' @param df A data frame containing the p-values or adjusted p-values.
#' @param filter_pvalue_or_fdr A character string indicating the column to filter by. Valid
#'   values are "p.value" or "p.adj" (case-insensitive). Default is "p.value".
#' @param significance_threshold A numeric value specifying the significance threshold. Rows with
#'   values less than this threshold are retained. Default is 0.05.
#'
#' @return A data frame filtered to include only rows where the specified column's
#'   values are below the threshold.
#' @export
#'
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   gene = c("Gene1", "Gene2", "Gene3"),
#'   p.value = c(0.01, 0.05, 0.1),
#'   p.adj = c(0.05, 0.1, 0.2)
#' )
#'
#' # Filter by p.value < 0.01
#' filter_by_pvalue_fdr(df, "p.value", 0.051)
#'
#' # Filter by p.adj < 0.05
#' filter_by_pvalue_fdr(df, "p.adj", 0.051)
#'
filter_by_pvalue_fdr <- function(df, filter_pvalue_or_fdr, significance_threshold){
  if (stringr::str_to_lower(filter_pvalue_or_fdr) %in% c("p.value", "p.adj")){
    df_f <- df[df[,filter_pvalue_or_fdr] < significance_threshold,]
  } else {
    warning("filter_pvalue_or_fdr should be either 'p.value' or 'p.adj'. Defaulting to 'p.value'")
    df_f <- df %>% filter(p.value < significance_threshold)
  }
  return(df_f)
}
