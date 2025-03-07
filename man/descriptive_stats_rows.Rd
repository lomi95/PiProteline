% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_stats_rows.R
\name{descriptive_stats_rows}
\alias{descriptive_stats_rows}
\title{Compute Descriptive Statistics Row-wise}
\usage{
descriptive_stats_rows(data)
}
\arguments{
\item{data}{A data frame containing numeric columns.}
}
\value{
A data frame containing descriptive statistics: Count, Missing, Mean,
  Standard Deviation (SD), Median, Minimum (Min), and Maximum (Max) for each row.
}
\description{
This function computes basic descriptive statistics for each row in a data frame,
treating zeros as missing (NA) values.
}
\examples{
df <- data.frame(a = c(1, 0, 3), b = c(4, 5, 0), c = c("x", "y", "z"))
descriptive_stats_rows(df)
}
