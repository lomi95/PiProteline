% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfList_on_excel.R
\name{dfList_on_excel}
\alias{dfList_on_excel}
\title{Write a list of data frames to an Excel workbook}
\usage{
dfList_on_excel(df_list, wb = NULL, name_sheets = "", rowNames = T)
}
\arguments{
\item{df_list}{A named list of data frames. If unnamed, default names are assigned.}

\item{wb}{A `Workbook` object created with `openxlsx::createWorkbook()`. If `NULL`, a new workbook is created.}

\item{name_sheets}{A character string appended to each sheet name. Defaults to `""`.}

\item{rowNames}{A logical value indicating whether to include row names. Defaults to `TRUE`.}
}
\value{
A `Workbook` object with the data frames written as sheets.
}
\description{
This function writes a named list of data frames to an Excel workbook.
Each element in the list is written to a separate sheet.
}
\examples{
\dontrun{
library(openxlsx)
df_list <- list(iris = iris, mtcars = mtcars)
wb <- dfList_on_excel(df_list)
saveWorkbook(wb, "output.xlsx", overwrite = TRUE)
}
}
