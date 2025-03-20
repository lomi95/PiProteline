testthat::test_that("check_taboo_words allows non-taboo words", {
  testthat::expect_no_error(check_taboo_words(c("gene", "expression")))
})

testthat::test_that("check_taboo_words stops on taboo words", {
  testthat::expect_error(check_taboo_words(c("p_value", "gene")),
                         "Any of the following words can't be used")
})

testthat::test_that("check_taboo_words is case-insensitive", {
  testthat::expect_error(check_taboo_words(c("P_VALUE")),
                         "Any of the following words can't be used")
})

testthat::test_that("check_taboo_words detects substrings", {
  testthat::expect_error(check_taboo_words(c("centrality-based")),
                         "Any of the following words can't be used")
})
