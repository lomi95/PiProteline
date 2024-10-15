test_that("transpose works with default gene_column = 1", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene3"),
    Sample1 = c(1, 4, 7),
    Sample2 = c(2, 5, 8),
    Sample3 = c(3, 6, 9)
  )

  result <- transpose(data)

  # Check if the resulting data frame has correct dimensions
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)

  # Check if the column names are as expected
  expect_equal(colnames(result), c("Gene1", "Gene2", "Gene3"))
})

test_that("transpose works with gene_column = 0 (using row names)", {
  data <- data.frame(
    Sample1 = c(1, 4, 7),
    Sample2 = c(2, 5, 8),
    Sample3 = c(3, 6, 9)
  )

  rownames(data) <- c("Gene1", "Gene2", "Gene3")

  result <- transpose(data, gene_column = 0)

  # Check if the resulting data frame has correct dimensions
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)

  # Check if the column names are the original row names
  expect_equal(colnames(result), c("Gene1", "Gene2", "Gene3"))
})

test_that("transpose works with specified gene_column by name", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene3"),
    Sample1 = c(1, 4, 7),
    Sample2 = c(2, 5, 8),
    Sample3 = c(3, 6, 9)
  )

  result <- transpose(data, gene_column = "Gene")

  # Check if the resulting data frame has correct dimensions
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)

  # Check if the column names are as expected
  expect_equal(colnames(result), c("Gene1", "Gene2", "Gene3"))
})

test_that("transpose handles non-existing gene_column gracefully", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene3"),
    Sample1 = c(1, 4, 7),
    Sample2 = c(2, 5, 8),
    Sample3 = c(3, 6, 9)
  )

  expect_error(transpose(dataset = data, gene_column = "NonExistent"),
               "undefined columns selected")
  expect_error(transpose(dataset = data, gene_column = 10),
               "undefined columns selected")
})

test_that("transpose works with gene_column as a numeric index other than 1", {
  data <- data.frame(
    Sample1 = c(1, 4, 7),
    Sample2 = c(2, 5, 8),
    Gene = c("Gene1", "Gene2", "Gene3"),
    Sample3 = c(3, 6, 9)
  )

  result <- transpose(dataset = data, gene_column = 3)

  # Check if the resulting data frame has correct dimensions
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)

  # Check if the column names are as expected
  expect_equal(colnames(result), c("Gene1", "Gene2", "Gene3"))
})
