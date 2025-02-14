#' Write a List of Data Frames to an Excel Sheet
#'
#' This function writes a list of data frames (`df_list`) to a specified sheet (`name_sheet`)
#' in an Excel workbook (`wb`). Each data frame is placed either in columns or in rows, depending on `by_col`.
#'
#' @param df_list A named or unnamed list of data frames to be written to the Excel sheet.
#' @param wb An optional `openxlsx` workbook object. If `NULL`, a new workbook is created.
#' @param name_sheet A character string specifying the name of the sheet where the data will be written.
#'   Default is `"Sheet"`.
#' @param by_col A logical value indicating whether data frames should be arranged column-wise (`TRUE`)
#'   or row-wise (`FALSE`). Default is `TRUE`.
#' @param rowNames A logical value indicating whether row names should be included in the output.
#'   Default is `TRUE`.
#' @param n_space An integer specifying the number of empty spaces between tables when arranged
#'   in columns (`by_col = TRUE`) or rows (`by_col = FALSE`). Default is `1`.
#'
#' @return A modified `openxlsx` workbook object (`wb`) with the data written to the specified sheet.
#' @export
#' @importFrom openxlsx createWorkbook addWorksheet writeData writeDataTable
#' @examples
#' \dontrun{
#' library(openxlsx)
#' df1 <- data.frame(A = 1:5, B = 6:10)
#' df2 <- data.frame(X = 11:15, Y = 16:20)
#' df_list <- list(Data1 = df1, Data2 = df2)
#' wb <- dfList_on_sheet(df_list)
#' saveWorkbook(wb, "example.xlsx", overwrite = TRUE)
#' }
#'
dfList_on_sheet <- function(df_list, wb = NULL, name_sheet = "Sheet", by_col = T, rowNames = T, n_space = 1){
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

  addWorksheet(wb, substr(name_sheet,1,31))

  if (by_col == T){
    lapply(seq_along(df_list), function(x){
      writeData(
        wb, name_sheet,
        names(df_list)[x], rowNames = rowNames,
        startCol = (x - 1) * (ncol(df_list[[x]]) + n_space + rowNames) + 1
      )
      writeDataTable(
        wb, name_sheet,
        df_list[[x]], rowNames = rowNames, startRow = 2,
        startCol = (x - 1) * (ncol(df_list[[x]]) + n_space + rowNames) + 1
      )
    })
  } else {
    lapply(seq_along(df_list), function(x){
      writeData(
        wb, name_sheet,
        names(df_list)[x], rowNames = rowNames,
        startRow = (x - 1) * (nrow(df_list[[x]]) + n_space) + 3
      )
      writeDataTable(
        wb, name_sheet,
        df_list[[x]], rowNames = rowNames,
        startRow = (x - 1) * (nrow(df_list[[x]]) + n_space) + 3
      )
    })
  }

  return(wb)
}
