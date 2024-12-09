% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volcano_plot.R
\name{volcano_plot}
\alias{volcano_plot}
\title{Volcano plot for Differential Expression Analysis}
\usage{
volcano_plot(df, boundFC, thr_pvalue, Name_1 = "Group1", Name_2 = "Group2")
}
\arguments{
\item{df}{A data frame with columns named `log2` (log2 fold change, x-axis), `pv` (-log10 of the p-value, y-axis), and `gene` (gene names or labels).}

\item{boundFC}{A numeric vector of length 2 specifying the fold change boundaries. Genes with log2 fold changes outside this range are highlighted in red (up-regulated) or blue (down-regulated).}

\item{thr_pvalue}{A numeric value indicating the p-value threshold. Genes with p-values below this threshold (after applying -log10 transformation) are considered significant.}

\item{Name_1}{A character string specifying the name of the first group. Default is "Group1".}

\item{Name_2}{A character string specifying the name of the second group. Default is "Group2".}
}
\value{
A list containing:
\describe{
  \item{plot}{A `ggplot` object representing the volcano plot.}
  \item{Genes}{A list of up-regulated and down-regulated gene names.}
}
}
\description{
This function generates a volcano plot to visualize differential expression analysis results, highlighting genes that are significantly up-regulated or down-regulated based on log-fold change and p-value thresholds.
}
\examples{
# Example data
example_data <- data.frame(
  log2 = rnorm(1000),
  pv = -log10(runif(1000)),
  gene = paste0("Gene", 1:1000)
)
# Generate volcano plot
result <- volcano_plot(example_data, boundFC = c(-1, 1), thr_pvalue = 0.05)
# Display the plot
print(result$plot)
# List up-regulated and down-regulated genes
result$Genes

}
