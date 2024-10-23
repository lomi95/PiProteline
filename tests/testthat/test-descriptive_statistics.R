test_that("descriptive_statistics computes statistics correctly", {
  data.unique <- data.frame(
    Sample1 = c(10, 20, 30, 56, 98),
    Sample2 = c(15, 25, 35, 11, 55),
    Sample3 = c(5, 15, 25, 654, 79),
    Sample4 = c(5, 123, 215, 36, 41),
    Sample5 = c(12,12, 12, 11, 87)
  )

  data.grouped <- list(
    Group1 = data.unique[1:2, ],
    Group2 = data.unique[2:3, ]
  )

  data.grouped.full <- list(
    Group1 = data.unique,
    Group2 = data.unique
  )

  result <- descriptive_statistics(data.unique, data.grouped, data.grouped.full)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that all required elements are present in the output
  expect_true(all(c("DS_col", "DS_row", "DS_col.groups",
                    "DS_row.groups", "CorrBetweenGroups") %in% names(result)))

  # Check if descriptive statistics for columns are data frames
  expect_s3_class(result$DS_col, "data.frame")
  expect_s3_class(result$DS_row, "data.frame")

  # Check that the correlation analysis result is not NULL
  expect_false(is.null(result$CorrBetweenGroups))
})

test_that("descriptive_statistics handles different groupings correctly", {
  data.unique <- data.frame(
    Sample1 = c(10, 20, 30, 56, 98),
    Sample2 = c(15, 25, 35, 11, 55),
    Sample3 = c(5, 15, 25, 654, 79),
    Sample4 = c(5, 123, 215, 36, 41),
    Sample5 = c(12,12, 12, 11, 87)
  )
  data.grouped <- list(
    Group1 = data.unique[1:2, ],
    Group2 = data.unique[2:3, ]
  )

  data.grouped.full <- list(
    Group1 = data.unique,
    Group2 = data.unique
  )

  result <- descriptive_statistics(data.unique, data.grouped, data.grouped.full)

  # Check if the length of DS_col.groups matches the number of groups
  expect_equal(length(result$DS_col.groups), length(data.grouped))

  # Check if the length of DS_row.groups matches the number of groups
  expect_equal(length(result$DS_row.groups), length(data.grouped))

  # Verify that DS_col.groups are lists of data frames
  expect_true(all(sapply(result$DS_col.groups, is.data.frame)))

  # Verify that DS_row.groups are lists of data frames
  expect_true(all(sapply(result$DS_row.groups, is.data.frame)))
})


test_that("descriptive_statistics checks for valid input", {
  data.unique <- data.frame(
    Sample1 = c(10, 20, 30),
    Sample2 = c(15, 25, 35),
    Sample3 = c(5, 15, 25)
  )

  expect_error(descriptive_statistics(NULL, data.grouped, data.grouped.full),
               "Input must be a data frame.")
})

