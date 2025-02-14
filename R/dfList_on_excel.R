#' Write a list of data frames to an Excel workbook
#'
#' This function writes a named list of data frames to an Excel workbook.
#' Each element in the list is written to a separate sheet.
#'
#' @param df_list A named list of data frames. If unnamed, default names are assigned.
#' @param wb A `Workbook` object created with `openxlsx::createWorkbook()`. If `NULL`, a new workbook is created.
#' @param name_sheets A character string appended to each sheet name. Defaults to `""`.
#' @param rowNames A logical value indicating whether to include row names. Defaults to `TRUE`.
#'
#' @return A `Workbook` object with the data frames written as sheets.
#' @export
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeDataTable
#' @examples
#' \dontrun{
#' library(openxlsx)
#' df_list <- list(iris = iris, mtcars = mtcars)
#' wb <- dfList_on_excel(df_list)
#' saveWorkbook(wb, "output.xlsx", overwrite = TRUE)
#' }

dfList_on_excel <- function(df_list, wb = NULL, name_sheets = "", rowNames = T){
  if (is.null(wb)){
    wb <- createWorkbook()
  }

  if (!is.null(names(df_list))){
    names_list <- names(df_list)

  } else {
    names_list <- paste0("Sheet_df",seq_along(df_list))
    names(df_list) <- names_list
  }
  names(names_list) <- names_list

  lapply(names_list, function(x){
    addWorksheet(wb, substr(paste0(x, name_sheets),1,31))
    writeDataTable(wb, paste0(x, name_sheets),
                   df_list[[x]], rowNames = rowNames)
  })

  return(wb)
}
