#' Intersect Enrichment Results
#'
#' This function identifies common enrichment terms between two enrichment result lists, optionally accounting for opposite trends.
#'
#' @param enr1 A list of enrichment results, where each element is a data frame with enrichment terms.
#' @param enr2 A list of enrichment results, where each element is a data frame with enrichment terms. Must be of the same length as `enr1`.
#' @param oppositeTrend A logical value indicating whether to identify enrichment terms with opposite trends between `enr1` and `enr2`. Default is `TRUE`.
#'
#' @return A list containing:
#' \describe{
#'   \item{enrichment_SameTrend}{A list of common enrichment terms between `enr1` and `enr2` for the same trend.}
#'   \item{enrichment_OppositeTrend}{If `oppositeTrend` is `TRUE`, a list of common enrichment terms where the trend is opposite between `enr1` and `enr2`.}
#' }
#' If `oppositeTrend` is `FALSE`, only `enrichment_SameTrend` is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' enr1 <- list(
#'   Group1 = data.frame(term = c("Term1", "Term2"),
#'                       description = c("Desc1", "Desc2"),
#'                       category = c("Cat1", "Cat2")),
#'   Group2 = data.frame(term = c("Term3", "Term4"),
#'                       description = c("Desc3", "Desc4"),
#'                       category = c("Cat3", "Cat4"))
#' )
#' enr2 <- list(
#'   Group1 = data.frame(term = c("Term1", "Term5"),
#'                       description = c("Desc1", "Desc5"),
#'                       category = c("Cat1", "Cat5")),
#'   Group2 = data.frame(term = c("Term3", "Term6"),
#'                       description = c("Desc3", "Desc6"),
#'                       category = c("Cat3", "Cat6"))
#' )
#' intersect_enrichment(enr1, enr2, oppositeTrend = TRUE)
#' }
intersect_enrichment <- function(enr1, enr2, oppositeTrend = TRUE) {

  if (length(enr1) != length(enr2)) {
    stop("enr1 and enr2 must have the same length")
  }

  if (any(names(enr1) != names(enr2))) {
    warning("names of enr1 and enr2 differ")
  }

  enr_sameTrend <- list()
  sq <- seq_along(enr1)

  # Trovare i termini comuni tra enr1 e enr2 con lo stesso trend
  for (i in sq) {
    term.intST <- intersect(enr1[[i]]$term, enr2[[i]]$term)
    enr_sameTrend[[i]] <- enr1[[i]] %>% dplyr::filter(term %in% term.intST) %>%
      dplyr::select(term, description, category)
  }
  names(enr_sameTrend) <- names(enr1)

  # Trovare i termini comuni con trend opposto, se richiesto
  if (oppositeTrend) {
    enr_oppositeTrend <- list()

    for (i in sq[seq(1, length(sq), by = 2)]) {
      term.intOT_1 <- intersect(enr1[[i]]$term, enr2[[i + 1]]$term)
      enr_oppositeTrend[[i]] <- enr1[[i]] %>% dplyr::filter(term %in% term.intOT_1) %>%
        dplyr::select(term, description, category)

      term.intOT_2 <- intersect(enr1[[i + 1]]$term, enr2[[i]]$term)
      enr_oppositeTrend[[i + 1]] <- enr1[[i + 1]] %>% dplyr::filter(term %in% term.intOT_2) %>%
        dplyr::select(term, description, category)
    }
    names(enr_oppositeTrend) <- names(enr1)

    return(list(enrichment_SameTrend = enr_sameTrend, enrichment_OppositeTrend = enr_oppositeTrend))
  }

  return(enr_sameTrend)
}
