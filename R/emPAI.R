#' emPAI
#'
#' @param list.groups_peptides list of groups of which we want to compute emPAI index, or a dataset Sample x Protein
#' @param Pn_ideal vector of ideal peptides
#'
#' @return The emPAI matrix/vector for each protein
#' @export
#'
emPAI_function <- function(list.groups_peptides, Pn_ideal){

  # Pept osservabili = siti tripsina = ARGININA/LISINA & 5/6 AA di lunghezza peptide
  # Pept osservati

  if (is.list(list.groups_peptides)){
    Empai <- sapply(list.groups_peptides, function(x){
      (10^(colMeans(x)/Pn_ideal-1))/sum(10^(colMeans(x)/Pn_ideal-1))
    })
  } else {
    Empai <- (10^(colMeans(list.groups_peptides)/Pn_ideal-1))/sum(10^(colMeans(list.groups_peptides)/Pn_ideal-1))
  }



  return(Empai)
}
