% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrichment_Netw.R
\name{enrichment_Netw}
\alias{enrichment_Netw}
\title{Network Enrichment Analysis}
\usage{
enrichment_Netw(
  prot_UnwNetw,
  prot_WNetw,
  names_of_groups,
  tax_ID,
  categories,
  differences = TRUE
)
}
\arguments{
\item{prot_UnwNetw}{A list of protein sets derived from unweighted networks, grouped by specified categories.}

\item{prot_WNetw}{A list of protein sets derived from weighted networks, grouped by specified categories.}

\item{names_of_groups}{A character vector specifying the names of the groups to analyze.}

\item{tax_ID}{An integer representing the taxonomy ID for the organism (e.g., 9606 for human).}

\item{categories}{A character vector specifying the enrichment categories to filter (e.g., "GO", "KEGG").}

\item{differences}{A logical value indicating whether to calculate differential enrichment between groups. Default is `TRUE`.}
}
\value{
If `differences` is `TRUE`, a list containing:
\describe{
  \item{enr_Netw}{A list of enrichment results for each group.}
  \item{enr_Netw.diff}{A list of differential enrichment results between each pair of groups.}
}
If `differences` is `FALSE`, it returns a list of enrichment results for each group.
}
\description{
This function performs enrichment analysis for protein groups from unweighted and weighted networks, using the STRING database. It identifies enrichment categories and computes differential enrichment between groups.
}
\examples{
\dontrun{
# Example usage
prot_UnwNetw <- list(
  Group1 = c("P12345", "Q67890"),
  Group2 = c("P54321", "Q09876")
)
prot_WNetw <- list(
  Group1 = c("A12345", "B67890"),
  Group2 = c("A54321", "B09876")
)
names_of_groups <- c("Group1", "Group2")
tax_ID <- 9606
categories <- c("GO", "KEGG")

enrichment_Netw(prot_UnwNetw, prot_WNetw, names_of_groups, tax_ID, categories, diff = TRUE)
}
}
