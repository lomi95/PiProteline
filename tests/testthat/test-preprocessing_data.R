test_that("preprocessing_data works with default settings", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Group1 = c(10, 20, 30, 40),
    Group2 = c(15, 25, 35, 45),
    Group3 = c(5, 15, 25, 35)
  )

  result <- preprocessing_data(data, names_of_groups = c("Group1", "Group2", "Group3"),
                               gene_column = "Gene", normType = "Znorm")

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that all required elements are present in the output
  expect_true(all(c("data.unique", "data.grouped", "data.grouped.full",
                    "data.grouped.evenDim", "data.norm") %in% names(result)))

})

test_that("preprocessing_data handles different normalization methods", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Group1 = c(10, 20, 30, 40),
    Group2 = c(15, 25, 35, 45),
    Group3 = c(5, 15, 25, 35)
  )

  norm_types <- c("ln", "Znorm", "MinMax", "Robust", "UnitVector",
                  "TotSigNorm", "MaxSigNorm", "RowSigmaNorm")

  for (norm in norm_types) {
    result <- preprocessing_data(data, names_of_groups = c("Group1", "Group2", "Group3"),
                                 gene_column = "Gene", normType = norm)

    # Check if the normalized data is a data frame
    expect_s3_class(result$data.norm, "data.frame")

  }
})

test_that("preprocessing_data handles additional arguments for remove_duplicates", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Group1 = c(10, 20, 30, 40),
    Group2 = c(15, 25, 35, 45),
    Group3 = c(5, 15, 25, 35)
  )

  result <- preprocessing_data(data, names_of_groups = c("Group1", "Group2", "Group3"),
                               gene_column = "Gene", normType = "Znorm", NAasZero = TRUE)

  # Verify that NAasZero argument is applied in remove_duplicates
  expect_false(any(is.na(result$data.unique)))
})

test_that("preprocessing_data handles invalid gene_column gracefully", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Group1 = c(10, 20, 30, 40),
    Group2 = c(15, 25, 35, 45),
    Group3 = c(5, 15, 25, 35)
  )

  expect_error(preprocessing_data(data, names_of_groups = c("Group1", "Group2", "Group3"),
                                  gene_column = "InvalidColumn", normType = "Znorm"),
               "undefined columns selected")
})

test_that("preprocessing_data works with no normalization applied", {
  data <- data.frame(
    Gene = c("Gene1", "Gene2", "Gene1", "Gene3"),
    Group1 = c(10, 20, 30, 40),
    Group2 = c(15, 25, 35, 45),
    Group3 = c(5, 15, 25, 35)
  )

  result <- preprocessing_data(data, names_of_groups = c("Group1", "Group2", "Group3"),
                               gene_column = "Gene", normType = NULL)

  # Verify that the normalization step didn't alter the original data structure
  expect_equal(dim(result$data.norm), dim(result$data.unique))
})
