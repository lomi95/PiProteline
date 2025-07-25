#' wrap text after a space and number of characters
#'
#' @param text a character vector to wrap
#' @param n an integer specifying the number of characters after which to wrap the text
#'
#' @returns a character vector with wrapped text
#'
wrap_after_space <- function(text, n = 20) {
  sapply(text, function(x) {
    # Se il testo è più corto di 1.5 * n, non inserire \n
    if (nchar(x) <= 1.4 * n) return(x)

    # Cerca il primo spazio dopo il carattere n
    rest <- substr(x, n + 1, nchar(x))
    space_pos <- regexpr(" ", rest)[1]

    # Se non ci sono spazi, restituisci il testo originale
    if (space_pos == -1) return(x)

    # Calcola il punto di taglio e inserisci \n
    cut_point <- n + space_pos
    paste0(substr(x, 1, cut_point - 1), "\n", substr(x, cut_point + 1, nchar(x)))
  }, USE.NAMES = FALSE)
}

