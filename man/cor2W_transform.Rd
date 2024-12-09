% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor2W_transform.R
\name{cor2W_transform}
\alias{cor2W_transform}
\title{cor2W_transform}
\usage{
cor2W_transform(flattCorr, signifCorr)
}
\arguments{
\item{flattCorr}{A data frame representing the flattened correlation matrix, containing
columns for correlation values ('cor') and adjusted p-values ('p.adj').}

\item{signifCorr}{A numeric value specifying the significance threshold for p-values.}
}
\value{
A data frame with the original correlation matrix data and an additional
  'weights' column representing the transformed weights.
}
\description{
Transforms a flattened correlation matrix data frame by adding a 'weights' column.
The transformation considers the correlation values and adjusts them based on
significance thresholds.
}
\examples{
# Example 1: Basic usage with a small data frame
flattCorr <- data.frame(
  cor = c(0.8, 0.5, -0.3, 0.9),
  p.adj = c(0.05, 0.02, 1, 0.8)
)
signifCorr <- 0.05

# Apply the transformation
transformed_data <- cor2W_transform(flattCorr, signifCorr)
print(transformed_data)

# Example 2: When all p-values are below the significance threshold
flattCorr <- data.frame(
  cor = c(0.6, 0.7, 0.2, -0.4),
  p.adj = c(0.01, 0.03, 0.02, 0.04)
)
signifCorr <- 0.05

# No weights should be modified since all p-values are significant
transformed_data <- cor2W_transform(flattCorr, signifCorr)
print(transformed_data)

# Example 3: All p-values are above the significance threshold
flattCorr <- data.frame(
  cor = c(0.3, -0.5, 0.4, 0.6),
  p.adj = c(0.2, 0.7, 0.9, 0.8)
)
signifCorr <- 0.05

# In this case, the weights should be transformed for all rows
transformed_data <- cor2W_transform(flattCorr, signifCorr)
print(transformed_data)

# Example 4: Mixed p-values, some above and some below the significance threshold
flattCorr <- data.frame(
  cor = c(0.5, -0.2, 0.9, 0.7),
  p.adj = c(0.01, 0.2, 0.04, 0.8)
)
signifCorr <- 0.05

# Only the weights for rows 2 and 4 should be transformed
transformed_data <- cor2W_transform(flattCorr, signifCorr)
print(transformed_data)

}
