% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_interactome.R
\name{build_interactome}
\alias{build_interactome}
\title{Build Interactome from StringDB Data}
\usage{
build_interactome(
  directory_interactome = NULL,
  tax_ID,
  interactome = NULL,
  scores_threshold = NULL,
  splitting_for_API = 1999
)
}
\arguments{
\item{directory_interactome}{A string specifying the directory of the StringDB interactome file.
This parameter is ignored if `interactome` is provided.}

\item{tax_ID}{An integer specifying the taxonomy ID of the species for annotation.}

\item{interactome}{A data frame of interactome data. Default is `NULL`, in which case
the function will read the file specified by `directory_interactome`.}

\item{scores_threshold}{A numeric vector of threshold scores to filter the interactome by.
If `NULL`, no filtering is applied.}

\item{splitting_for_API}{An integer vector for splitting the protein list during annotation to
avoid API timeout errors. Lower values may prevent timeouts. Default is `1999`.}
}
\value{
A data frame representeing the interactome containing different scores for each couple of proteins.
}
\description{
This function builds a protein-protein interaction (PPI) network, or interactome,
from a StringDB interactome file. Optionally, it can filter the interactome by score
thresholds and annotate protein interactions using StringDB API.
}
\details{
The function first filters the input interactome based on the provided score thresholds.
It then retrieves the protein-protein interactions and annotations from the StringDB
database using the `protein_mapping` function.
}
\examples{
\dontrun{
  destfile <- "C:/Users/WKS/Downloads/STRINGDB_intractome.HS.txt.gx"
  download.file("https://stringdb-downloads.org/download/protein.links.full.v12.0/
                 9606.protein.links.full.v12.0.txt.gz",
                 destfile = destfile)
  interactome_hs <- build_interactome(directory_interactome = destfile,tax_ID = 9606)
}

}
\seealso{
\code{\link{protein_mapping}}, \code{\link{filter_interactome}}
}
