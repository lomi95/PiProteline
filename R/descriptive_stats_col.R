#' Compute Descriptive Statistics Column-wise
#'
#' This function computes basic descriptive statistics for each numeric column
#' in a data frame, treating zeros as missing (NA) values.
#'
#' @param data A data frame containing numeric columns.
#' @return A data frame containing descriptive statistics: Count, Missing, Mean,
#'   Standard Deviation (SD), Median, Minimum (Min), and Maximum (Max).
#' @examples
#' df <- data.frame(a = c(1, 0, 3), b = c(4, 5, 0), c = c("x", "y", "z"))
#' descriptive_stats_col(df)
#' @export
descriptive_stats_col <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  # Handle empty data frame
  if (nrow(data) == 0 || ncol(data) == 0) {
    message("Input data frame is empty. No statistics to compute.")
    return(data.frame())
  }

  numeric_data <- data[sapply(data, is.numeric)]
  numeric_data[numeric_data == 0] <- NA

  message("Descriptive statistics computed on non-missing data (including zeros converted to NA).")

  stats <- lapply(numeric_data, function(x) {
    if (all(is.na(x))) {
      c(
        Count = length(x),
        Missing = sum(is.na(x)),
        Mean = NA,
        SD = NA,
        Median = NA,
        Min = NA,
        Max = NA
      )
    } else {
      c(
        Count = length(x),
        Missing = sum(is.na(x)),
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        Min = ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE)),
        Max = ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
      )
    }
  })

  stats_df <- do.call(rbind, stats)
  return(as.data.frame(stats_df))
}
