% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Normalization.R
\name{Normalization}
\alias{Normalization}
\title{Data Normalization}
\usage{
Normalization(dataset, normType)
}
\arguments{
\item{dataset}{A data frame where rows represent proteins and columns represent samples.}

\item{normType}{A character string specifying the type of normalization. Choose from:
\describe{
  \item{ln}{Natural logarithm transformation}
  \item{Znorm}{Z-score normalization}
  \item{MinMax}{Min-Max scaling}
  \item{Robust}{Robust scaling using median and IQR}
  \item{UnitVector}{Unit vector scaling}
  \item{TotSigNorm}{Total signal normalization}
  \item{MaxSigNorm}{Maximum signal normalization}
  \item{RowSigmaNorm}{Row sigma normalization}
}}
}
\value{
A data frame with the normalized data.
}
\description{
This function normalizes a dataset (Proteins x Samples) using various normalization techniques. It supports different methods for scaling and transforming the data, including natural logarithm transformation, Z-score normalization, Min-Max scaling, and more.
}
\examples{
# Example dataset
data <- data.frame(
  Protein1 = c(10, 20, 30),
  Protein2 = c(5, 15, 25),
  Protein3 = c(2, 8, 18)
)

# Normalize using different methods
Normalization(data, normType = "ln")
Normalization(data, normType = "Znorm")
Normalization(data, normType = "MinMax")
Normalization(data, normType = "Robust")
Normalization(data, normType = "UnitVector")
Normalization(data, normType = "TotSigNorm")
Normalization(data, normType = "MaxSigNorm")
Normalization(data, normType = "RowSigmaNorm")

}
