#' Filter interactome data based on score thresholds
#'
#' Filters the interactome data frame or matrix by retaining rows where at least
#' one of the specified score columns exceeds the corresponding threshold.
#'
#' @param interactome A data frame containing interactome data. Each row
#'   represents an interaction, and columns represent different scores or metrics.
#' @param scores_threshold A named numeric vector of thresholds, where names
#'   correspond to column names in the `interactome`. Only rows where at least
#'   one score exceeds the respective threshold are retained. Default is
#'   \code{c("escore" = 150, "dscore" = 300)}.
#'
#' @return A filtered data frame where rows meet the threshold criteria.
#'
#' @examples
#' interactome.hs <- data.frame(protein1 = c("P12345", "P67890", "P23456", "P34567"),
#'                              protein2 = c("P54321", "P09876", "P65432", "P76543"),
#'                              experimental = c(200, 120, 180, 160),
#'                              database = c(400, 300, 350, 320))
#'
#' # Define custom score thresholds
#' score_thresholds <- c("experimental" = 150, "database" = 350)
#'
#' # Filter the interactome data using these thresholds
#' filtered_interactome <- filter_interactome(interactome.hs, score_thresholds)
#'
#' @importFrom dplyr filter across all_of cur_column
#' @importFrom magrittr %>%
#' @export
filter_interactome <- function(interactome, scores_threshold = c("escore" = 150, "dscore" = 300)) {


  if (length(scores_threshold)){
    if (!all(names(scores_threshold) %in% colnames(interactome))) {
      stop("The names of 'scores_threshold' do not match the column names of the interactome")
    }
    df_filtered <- interactome %>%
      dplyr::filter(rowSums(dplyr::across(dplyr::all_of(names(scores_threshold)),
                            ~ . > scores_threshold[cur_column()]),
                     na.rm = TRUE) >= 1)

    return(df_filtered)
  }
  return(interactome)


}
