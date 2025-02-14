#' Intersect Enrichment Results
#'
#' This function identifies common enrichment terms between two enrichment result lists, optionally accounting for opposite trends.
#'
#' @param enr1 A list of enrichment results, where each element is a data frame with enrichment terms.
#' @param enr2 A list of enrichment results, where each element is a data frame with enrichment terms. Must be of the same length as `enr1`.
#'
#' @return A list containing:
#' \describe{
#'   \item{enrichment_SameTrend}{A list of common enrichment terms between `enr1` and `enr2` for the same trend.}
#'   \item{enrichment_OppositeTrend}{If `opposite_trend` is `TRUE`, a list of common enrichment terms where the trend is opposite between `enr1` and `enr2`.}
#' }
#' If `opposite_trend` is `FALSE`, only `enrichment_SameTrend` is returned.
#' @export
#'
#' @importFrom purrr reduce
#' @importFrom dplyr select filter arrange
#' @importFrom magrittr %>%
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
#' intersect_enrichment(enr1, enr2, opposite_trend = TRUE)
#' }
intersect_enrichment <- function(enr1, enr2) {

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
    term.intST <- intersect(enr1[[i]][["term"]], enr2[[i]][["term"]])
    enr_sameTrend[[i]] <- enr1[[i]] %>% dplyr::filter(term %in% term.intST) %>%
      dplyr::select(term, description, category)
  }
  names(enr_sameTrend) <- names(enr1)

  # Trovare i termini comuni con trend opposto, se richiesto
  enr_oppositeTrend <- list()

  for (i in sq[seq(1, length(sq), by = 2)]) {
    term.intOT_1 <- intersect(enr1[[i]][["term"]], enr2[[i + 1]][["term"]])
    enr_oppositeTrend[[i]] <- enr1[[i]] %>% dplyr::filter(term %in% term.intOT_1) %>%
      dplyr::select(term, description, category)

    term.intOT_2 <- intersect(enr1[[i + 1]][["term"]], enr2[[i]][["term"]])
    enr_oppositeTrend[[i + 1]] <- enr1[[i + 1]] %>% dplyr::filter(term %in% term.intOT_2) %>%
      dplyr::select(term, description, category)

  }
  names(enr_oppositeTrend) <- names(enr1)

  summaryEnrichment <- list()
  for (i in sq[seq(1, length(sq), by = 2)]){
    names_groups <- strsplit(names(enr_sameTrend)[i], "_vs_")[[1]]
    updown <- c(paste0("DAPs up ", names_groups),
                paste0("CNs up ", names_groups))
    up_up <- cbind(enr_sameTrend[[i]],
                                matrix(1, nrow = nrow(enr_sameTrend[[i]]), ncol = 1),
                                matrix(0, nrow = nrow(enr_sameTrend[[i]]), ncol = 1),
                                matrix(1, nrow = nrow(enr_sameTrend[[i]]), ncol = 1),
                                matrix(0, nrow = nrow(enr_sameTrend[[i]]), ncol = 1))
    colnames(up_up) <- c("term", "description", "category", updown)

    dw_dw <- cbind(enr_sameTrend[[i+1]],
                                matrix(0, nrow = nrow(enr_sameTrend[[i+1]]), ncol = 1),
                                matrix(1, nrow = nrow(enr_sameTrend[[i+1]]), ncol = 1),
                                matrix(0, nrow = nrow(enr_sameTrend[[i+1]]), ncol = 1),
                                matrix(1, nrow = nrow(enr_sameTrend[[i+1]]), ncol = 1))
    colnames(dw_dw) <- c("term", "description", "category", updown)

    up_dw <- cbind(enr_oppositeTrend[[i]],
                                matrix(1, nrow = nrow(enr_oppositeTrend[[i]]), ncol = 1),
                                matrix(0, nrow = nrow(enr_oppositeTrend[[i]]), ncol = 1),
                                matrix(0, nrow = nrow(enr_oppositeTrend[[i]]), ncol = 1),
                                matrix(1, nrow = nrow(enr_oppositeTrend[[i]]), ncol = 1))
    colnames(up_dw) <- c("term", "description", "category", updown)

    dw_up <- cbind(enr_oppositeTrend[[i+1]],
                                  matrix(0, nrow = nrow(enr_oppositeTrend[[i+1]]), ncol = 1),
                                  matrix(1, nrow = nrow(enr_oppositeTrend[[i+1]]), ncol = 1),
                                  matrix(1, nrow = nrow(enr_oppositeTrend[[i+1]]), ncol = 1),
                                  matrix(0, nrow = nrow(enr_oppositeTrend[[i+1]]), ncol = 1))
    colnames(dw_up) <- c("term", "description", "category", updown)






    enr_allTrend <- list(up_up, dw_dw, up_dw, dw_up)
    summaryEnrichment[[names(enr_sameTrend)[i]]] <- purrr::reduce(enr_allTrend, merge, by = colnames(up_dw), all = T) %>%
      dplyr::arrange(desc(.data[[updown[1]]]), desc(.data[[updown[3]]]), all_of(category), all_of(description))

  }

  return(summaryEnrichment)
}
