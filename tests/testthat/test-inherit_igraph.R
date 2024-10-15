test_that("inherit_igraph works correctly", {

  library(igraph)

  # Caso corretto: l'input è un oggetto igraph
  g <- make_ring(10)
  expect_silent(inherit_igraph(g))  # Non dovrebbe generare errori

  # Caso errato: l'input non è un oggetto igraph
  vec <- 1:10
  expect_error(inherit_igraph(vec), "Input must be an igraph object.")
})
