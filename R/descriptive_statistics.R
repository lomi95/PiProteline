#' Descriptive Statistics and Correlation Analysis
#'
#' This function computes descriptive statistics for the input data, including per-column and per-row statistics for the original dataset and grouped datasets. It also calculates correlations between different groups.
#'
#' @param data_unique A data frame containing the unique data (e.g., after preprocessing).
#' @param data_grouped_full A list of data frames representing the fully grouped data for correlation analysis.
#' @param ... Additional arguments passed to the functions \code{\link{corr_between_groups}}
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{DS_col}{Descriptive statistics for columns of the original dataset.}
#'   \item{DS_row}{Descriptive statistics for rows of the original dataset.}
#'   \item{DS_col_groups}{A list of descriptive statistics for columns of each grouped dataset.}
#'   \item{DS_row_group}{A list of descriptive statistics for rows of each grouped dataset.}
#'   \item{corrBetweenGroups}{Correlation analysis results between different groups.}
#' }
#' @export
#'
#' @examples
#' # Example usage
#' data_unique <- data.frame(Sample1 = c(10, 20, 30, 56, 98),
#'                          Sample2 = c(15, 25, 35, 11, 55),
#'                          Sample3 = c(5, 15, 25, 654, 79),
#'                          Sample4 = c(5, 123, 215, 36, 41),
#'                          Sample5 = c(12,12, 12, 11, 87)
#'                          )
#'
#'
#' data_grouped_full <- list(
#'   Group1 = data_unique,
#'   Group2 = data_unique
#' )
#'
#' descriptive_statistics(data_unique, data_grouped_full)
#'
descriptive_statistics <- function(data_unique, data_grouped_full, ...) {

  args_list <- list(...)

  DS_col <- suppressMessages(descriptive_stats_cols(data_unique))
  DS_row <- suppressMessages(descriptive_stats_rows(data_unique))
  DS_col_groups <- lapply(data_grouped_full, function(x) suppressMessages(descriptive_stats_cols(x)))
  DS_row_groups <- lapply(data_grouped_full, function(x) suppressMessages(descriptive_stats_rows(x)))

  # Calculate correlation between different groups
  args_corr_between_groups <- args_list[intersect(names(args_list),
                                                  names(formals(corr_between_groups)))]
  corrBetweenGroups <- do.call(corr_between_groups,
                               c(list(data_grouped = data_grouped_full), args_corr_between_groups))

  return(list(DS_col = DS_col,
              DS_row = DS_row,
              DS_col_groups = DS_col_groups,
              DS_row_groups = DS_row_groups,
              corrBetweenGroups = corrBetweenGroups))
}
