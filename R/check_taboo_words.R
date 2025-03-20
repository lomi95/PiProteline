#' Check for Taboo Words in Character Strings
#'
#' This function checks whether any element of a character vector contains taboo words.
#' If a taboo word is found, the function throws an error.
#'
#' @param char A character vector to be checked.
#'
#' @return None. The function is called for its side effect of stopping execution if taboo words are found.
#'
#' @details
#' The function is case-insensitive and checks if any taboo word is present as a substring within the input characters.
#' Taboo words include: "fc", "specific", "centrality", "weighted", "number_of_genes", "p_value", "fdr", and "input_genes".
#'
#' @examples
#' # Example where no taboo words are present
#' check_taboo_words(c("gene", "expression"))
#'
#' # Example where taboo words are present (will stop with an error)
#' \dontrun{
#' check_taboo_words(c("p_value", "gene"))
#' }
#'
#' @importFrom stringr str_to_lower str_c
#' @export

check_taboo_words <- function(char){
  tabooWords <- c("fc","specific", "centrality","weighted","number_of_genes","p_value","fdr","input_genes")
  check <- unlist(sapply(stringr::str_to_lower(char), function(x) sapply(tabooWords,grepl,x)))
  if (any(check)){
    stop("Any of the following words can't be used, not even in part:\n", stringr::str_c(tabooWords, collapse = ", "))
  }
}

