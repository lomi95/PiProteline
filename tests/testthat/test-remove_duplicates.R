test_that("remove_duplicates works with default settings", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Value1 = c(5, 2, 7, 3),
    Value2 = c(NA, 4, 1, 8)
  )

  result <- remove_duplicates(data, genes = "Gene")

  # Check dimensions of the result
  expect_equal(nrow(result), 3)

  # Verify that duplicate "Gene1" was removed
  expect_equal(result$GeneName, c("Gene1", "Gene2", "Gene3"))
})

test_that("remove_duplicates handles NAasZero correctly", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Value1 = c(5, 2, 7, 3),
    Value2 = c(NA, 4, 1, 8)
  )

  result <- remove_duplicates(data, genes = "Gene", NAasZero = TRUE)

  # Verify that NA values were replaced with zeros and computed correctly
  expect_equal(result$Value2[1], 1)
})

test_that("remove_duplicates handles ZeroasNA correctly", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Value1 = c(5, 2, 7, 3),
    Value2 = c(0, 4, 1, 8)
  )

  result <- suppressWarnings(remove_duplicates(data, genes = "Gene", NAasZero = F, ZeroasNA = TRUE))

  # Verify that zero values were replaced with NAs
  expect_true(is.na(result$Value2[1]))
})

test_that("remove_duplicates returns an error if both NAasZero and ZeroasNA are TRUE", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Value1 = c(5, 2, 7, 3),
    Value2 = c(NA, 4, 1, 8)
  )

  expect_error(remove_duplicates(data, genes = "Gene", NAasZero = TRUE, ZeroasNA = TRUE),
               "Both 'NAasZero' and 'ZeroasNA' are TRUE")
})

test_that("remove_duplicates works with numeric genes index", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Value1 = c(5, 2, 7, 3),
    Value2 = c(NA, 4, 1, 8)
  )

  result <- remove_duplicates(data, genes = 1)

  # Check dimensions of the result
  expect_equal(nrow(result), 3)

  # Verify that duplicate "Gene1" was removed
  expect_equal(result$GeneName, c("Gene1", "Gene2", "Gene3"))
})

test_that("remove_duplicates returns an error with invalid genes input", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Value1 = c(5, 2, 7, 3),
    Value2 = c(NA, 4, 1, 8)
  )

  expect_error(remove_duplicates(dataset = data, genes = c("Gene", "Gene")),
               "Length of 'genes' differs from number of rows of the dataset")
})
