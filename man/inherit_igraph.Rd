% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inherit_igraph.R
\name{inherit_igraph}
\alias{inherit_igraph}
\title{Check if Input is an igraph Object}
\usage{
inherit_igraph(g)
}
\arguments{
\item{g}{The object to check. It is expected to be an `igraph` object.}
}
\value{
No return value. The function throws an error if `graph` is not an
    `igraph` object.
}
\description{
This helper function checks whether the input is an `igraph` object and raises
an error if it is not. This is useful for ensuring that functions working with
graph data structures only receive valid inputs.
}
\examples{
\dontrun{


library(igraph)
g <- make_ring(10)
inherit_igraph(g)  # No error, valid igraph object

inherit_igraph(1:10)  # Will throw an error
}

}
