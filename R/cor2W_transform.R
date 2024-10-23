#' cor2W_transform
#'
#' Transforms a flattened correlation matrix data frame by adding a 'weights' column.
#' The transformation considers the correlation values and adjusts them based on
#' significance thresholds.
#'
#' @param flattCorr A data frame representing the flattened correlation matrix, containing
#'   columns for correlation values ('cor') and adjusted p-values ('p.adj').
#' @param signifCorr A numeric value specifying the significance threshold for p-values.
#'
#' @return A data frame with the original correlation matrix data and an additional
#'   'weights' column representing the transformed weights.
#'
#' @examples
#' # Example 1: Basic usage with a small data frame
#' flattCorr <- data.frame(
#'   cor = c(0.8, 0.5, -0.3, 0.9),
#'   p.adj = c(0.05, 0.02, 1, 0.8)
#' )
#' signifCorr <- 0.05
#'
#' # Apply the transformation
#' transformed_data <- cor2W_transform(flattCorr, signifCorr)
#' print(transformed_data)
#'
#' # Example 2: When all p-values are below the significance threshold
#' flattCorr <- data.frame(
#'   cor = c(0.6, 0.7, 0.2, -0.4),
#'   p.adj = c(0.01, 0.03, 0.02, 0.04)
#' )
#' signifCorr <- 0.05
#'
#' # No weights should be modified since all p-values are significant
#' transformed_data <- cor2W_transform(flattCorr, signifCorr)
#' print(transformed_data)
#'
#' # Example 3: All p-values are above the significance threshold
#' flattCorr <- data.frame(
#'   cor = c(0.3, -0.5, 0.4, 0.6),
#'   p.adj = c(0.2, 0.7, 0.9, 0.8)
#' )
#' signifCorr <- 0.05
#'
#' # In this case, the weights should be transformed for all rows
#' transformed_data <- cor2W_transform(flattCorr, signifCorr)
#' print(transformed_data)
#'
#' # Example 4: Mixed p-values, some above and some below the significance threshold
#' flattCorr <- data.frame(
#'   cor = c(0.5, -0.2, 0.9, 0.7),
#'   p.adj = c(0.01, 0.2, 0.04, 0.8)
#' )
#' signifCorr <- 0.05
#'
#' # Only the weights for rows 2 and 4 should be transformed
#' transformed_data <- cor2W_transform(flattCorr, signifCorr)
#' print(transformed_data)
#'
#' @export

cor2W_transform <- function(flattCorr, signifCorr) {

  ### Normalization to remove 1s from p-values, making logarithmic transformation possible
  # The goal is to transform the p-value range from (significance; 1] to (0; 1)

  # If the maximum p-value is 1, set the new maximum to the average of 1 and the second-highest p-value
  if (max(flattCorr$p.adj) == 1) {
    new_max <- mean(c(1, max(flattCorr$p.adj[-which(flattCorr$p.adj == 1)])))
  } else {
    new_max <- max(flattCorr$p.adj)
  }

  # Calculate normalization parameters: (p-value + epsilon) * k = p_norm1
  # (signifCorr + epsilon) * k = signifCorr
  # (max(p-value) + epsilon) * k = new_max

  # Solve the system
  eps <- (new_max - max(flattCorr$p.adj)) * signifCorr / (signifCorr - new_max)
  # k   <- ( signifCorr - new_max ) / ( signifCorr - max(flattCorr$p.adj) ) oLD
  k <- new_max*(signifCorr - max(flattCorr$p.adj))/(signifCorr*new_max - (max(flattCorr$p.adj))^2)

  # Apply normalization only to non-significant p-values
  ind_notsig <- which(flattCorr$p.adj > signifCorr)
  norm1 <- (flattCorr$p.adj[ind_notsig] + eps) * k

  # Apply -log10 to invert low p-values into high weights and vice versa
  lg <- -log10(norm1)

  ## Normalize lg between 0 and 1; calculate parameters (lg + eps_log) * k_log = p_norm2
  # (MAX_lg + eps_log) * k_log = 1
  # (min(lg) + eps_log) * k_log = min(lg)
  MAX_lg <- -log10(signifCorr)

  # Solve the system
  eps_log <- (min(lg) - MAX_lg * min(lg)) / (min(lg) - 1)
  k_log <- (min(lg) - 1) / (min(lg) - MAX_lg)
  norm2 <- (lg + eps_log) * k_log

  # Multiply the correlation by norm2^3 for non-significant values
  flattCorr$weights <- flattCorr$cor
  flattCorr$weights[ind_notsig] <- flattCorr$cor[ind_notsig] * norm2^3

  return(flattCorr)
}
