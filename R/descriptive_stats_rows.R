#' Compute Descriptive Statistics Row-wise
#'
#' This function computes basic descriptive statistics for each row in a data frame,
#' treating zeros as missing (NA) values.
#'
#' @param data A data frame containing numeric columns.
#' @return A data frame containing descriptive statistics: Count, Missing, Mean,
#'   Standard Deviation (SD), Median, Minimum (Min), and Maximum (Max) for each row.
#' @examples
#' df <- data.frame(a = c(1, 0, 3), b = c(4, 5, 0), c = c("x", "y", "z"))
#' descriptive_stats_rows(df)
#' @export
descriptive_stats_rows <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Handle empty data frames
  if (nrow(data) == 0 || ncol(data) == 0) {
    warning("Input data frame is empty. No statistics to compute.")
    return(data.frame())
  }

  numeric_data <- data[sapply(data, is.numeric)]
  numeric_data[numeric_data == 0] <- NA

  message("Descriptive statistics computed on non-missing data (including zeros converted to NA).")

  stats <- apply(numeric_data, 1, function(x) {
    if (all(is.na(x))) {
      c(
        Count = 0,  # No valid numeric values
        Missing = length(x),  # All values are missing
        Mean = NA,
        SD = NA,
        Median = NA,
        Min = NA,
        Max = NA
      )
    } else {
      c(
        Count = sum(!is.na(x)),     # Number of non-missing values
        Missing = sum(is.na(x)),    # Number of missing values
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        Min = ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE)),
        Max = ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
      )
    }
  })

  stats_df <- as.data.frame(t(stats))
  return(stats_df)
}
