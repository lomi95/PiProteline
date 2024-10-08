% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MDS_plot.R
\name{MDS_plot}
\alias{MDS_plot}
\title{MDS plot}
\usage{
MDS_plot(
  dataset,
  names_of_groups,
  ignoreCase = T,
  pos.vectors_groups = NULL,
  n_tree = 5000
)
}
\arguments{
\item{dataset}{The dataset we want to classify, Samples x Proteins}

\item{names_of_groups}{The names of the classes in rownames(dataset), ignored if 'pos.vectors_groups' is not NULL}

\item{ignoreCase}{if TRUE the 'names_of_groups' search is not case sensitive. Default = T}

\item{pos.vectors_groups}{A list object with in each element there are row indexes for classes. Default NULL}

\item{n_tree}{number of random forest trees}
}
\value{
A MDS plot
}
\description{
MDS plot
}
