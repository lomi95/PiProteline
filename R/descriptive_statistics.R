#' Descriptive Statistics and Correlation Analysis
#'
#' This function computes descriptive statistics for the input data, including per-column and per-row statistics for the original dataset and grouped datasets. It also calculates correlations between different groups.
#'
#' @param data.unique A data frame containing the unique data (e.g., after preprocessing).
#' @param data.grouped A list of data frames, each representing data grouped by specified criteria.
#' @param data.grouped.full A list of data frames representing the fully grouped data for correlation analysis.
#' @param ... Additional arguments passed to the functions \code{\link{Corr_Between_Groups}}
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{DS_col}{Descriptive statistics for columns of the original dataset.}
#'   \item{DS_row}{Descriptive statistics for rows of the original dataset.}
#'   \item{DS_col.groups}{A list of descriptive statistics for columns of each grouped dataset.}
#'   \item{DS_row.groups}{A list of descriptive statistics for rows of each grouped dataset.}
#'   \item{CorrBetweenGroups}{Correlation analysis results between different groups.}
#' }
#' @export
#'
#' @examples
#' # Example usage
#' data.unique <- data.frame(Sample1 = c(10, 20, 30, 56, 98),
#'                          Sample2 = c(15, 25, 35, 11, 55),
#'                          Sample3 = c(5, 15, 25, 654, 79),
#'                          Sample4 = c(5, 123, 215, 36, 41),
#'                          Sample5 = c(12,12, 12, 11, 87)
#'                          )
#'
#' data.grouped <- list(
#'   Group1 = data.unique[1:2, ],
#'   Group2 = data.unique[2:3, ]
#' )
#'
#' data.grouped.full <- list(
#'   Group1 = data.unique,
#'   Group2 = data.unique
#' )
#'
#' descriptive_statistics(data.unique, data.grouped, data.grouped.full)
#'
descriptive_statistics <- function(data.unique, data.grouped, data.grouped.full, ...) {
  args_list <- list(...)
  DS_col <- suppressMessages(descriptive_stats_cols(data.unique))
  DS_row <- suppressMessages(descriptive_stats_rows(data.unique))
  DS_col.groups <- lapply(data.grouped, function(x) suppressMessages(descriptive_stats_cols(x)))
  DS_row.groups <- lapply(data.grouped, function(x) suppressMessages(descriptive_stats_rows(x)))

  # Calculate correlation between different groups
  args_corr_between_groups <- args_list[intersect(names(args_list),
                                                  names(formals(Corr_Between_Groups)))]
  CorrBetweenGroups <- do.call(Corr_Between_Groups,
                               c(list(data.grouped = data.grouped.full), args_corr_between_groups))

  return(list(DS_col = DS_col,
              DS_row = DS_row,
              DS_col.groups = DS_col.groups,
              DS_row.groups = DS_row.groups,
              CorrBetweenGroups = CorrBetweenGroups))
}
