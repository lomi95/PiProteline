% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_critical_nodes.R
\name{summary_critical_nodes}
\alias{summary_critical_nodes}
\title{Summary of Critical Nodes}
\usage{
summary_critical_nodes(output_CN, paste_colnames = "")
}
\arguments{
\item{output_CN}{A list containing the critical nodes output from the network analysis. The list is expected to have searching modes as top-level keys, followed by critical node types ("Hubs" or "Bottlenecks") and group names.}

\item{paste_colnames}{A character string to append to the column names of the resulting matrix. Default is an empty string.}
}
\value{
A list containing:
\describe{
  \item{prot_Netw}{A list where each element contains the critical nodes for a specific combination of searching mode, critical node type, and group.}
  \item{df_CN}{A data frame where rows represent unique critical nodes and columns indicate the presence of each node across various combinations of searching mode, critical node type, and group, with a final column for gene names.}
}
}
\description{
This function generates a summary of critical nodes (Hubs and Bottlenecks) identified in network analysis results for multiple groups. It creates a matrix indicating the presence of each critical node across different groups and searching modes.
}
\examples{
\dontrun{
names_of_groups <- c("Group1", "Group2")

fun_list <- c(betweenness, centroids, bridging_centrality)
preprocData <- preprocessing_data(dataset, names_of_groups,1, "TotSigNorm" )
UNA <- unweighted_network_analysis(names_of_groups = names_of_groups,
                                   data_unique = preprocData$data_unique,
                                   fun_list = fun_list)
WNA <- weighted_netowrk_analysis(data_grouped_even_dim = data_grouped_even_dim,
                                 names_of_groups = names_of_groups,
                                 fun_list = fun_list)
unweighted_CN <- UNA$CriticalNodes
weighted_CN   <- WNA$CriticalNodes

summary_critical_nodes(unweighted_CN, "_unweighted")
summary_critical_nodes(weighted_CN, "_weighted")

}
}
