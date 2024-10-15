########################################test1########################################
test_that("descriptive_stats_col works with numeric data", {
  # Create test data
  test_data <- data.frame(
    col1 = c(1, 2, 0, 4, 5),   # contains zeros
    col2 = c(10, 20, 0, 40, 50), # contains zeros
    col3 = c("A", "B", "C", "D", "E") # non-numeric column
  )

  # Apply the function
  result <- descriptive_stats_col(test_data)

  # Check the structure of the output
  expect_true(is.data.frame(result))

  # Check if the correct number of rows are returned (since only col1 and col2 are numeric)
  expect_equal(nrow(result), 2)

  # Check the output for specific mean values (ignoring zeros)
  expect_equal(result["col1", "Mean"], mean(c(1, 2, 4, 5)))
  expect_equal(result["col2", "Mean"], mean(c(10, 20, 40, 50)))
})

########################################test2########################################
test_that("descriptive_stats_col handles columns with only zeros", {
  zero_data <- data.frame(
    col1 = c(0, 0, 0),
    col2 = c(0, 0, 0)
  )

  result <- descriptive_stats_col(zero_data)

  # Check if all values are considered missing (NA)
  expect_equal(result["col1", "Missing"], 3)
  expect_equal(result["col2", "Missing"], 3)

  # Check if mean, min, and max return NA since all values are converted to NA
  expect_true(is.na(result["col1", "Mean"]))
  expect_true(is.na(result["col2", "Mean"]))
  expect_true(is.na(result["col1", "Min"]))
  expect_true(is.na(result["col2", "Max"]))
})
########################################test3########################################
test_that("descriptive_stats_col handles columns with missing values", {
  missing_data <- data.frame(
    col1 = c(1, 2, NA, 4, 5),
    col2 = c(10, NA, 0, 40, 50)
  )

  result <- descriptive_stats_col(missing_data)

  # Check if the Missing count is accurate
  expect_equal(result["col1", "Missing"], 1)  # col1 has one NA
  expect_equal(result["col2", "Missing"], 2)  # col2 has one NA and one zero (converted to NA)

  # Check if the mean calculation ignores NA values
  expect_equal(result["col1", "Mean"], mean(c(1, 2, 4, 5), na.rm = TRUE))  # NA is ignored
  expect_equal(result["col2", "Mean"], mean(c(10, 40, 50), na.rm = TRUE))  # NA and 0 are ignored
})
########################################test4########################################
test_that("descriptive_stats_col handles data frames with no numeric columns", {
  no_numeric_data <- data.frame(
    col1 = c("a", "b", "c"),
    col2 = c("x", "y", "z")
  )

  result <- descriptive_stats_col(no_numeric_data)

  # Since there are no numeric columns, the result should be an empty data frame
  expect_equal(nrow(result), 0)
})
########################################test5########################################
test_that("descriptive_stats_col handles data frames with mixed types", {
  mixed_data <- data.frame(
    col1 = c(1, 2, 0, 4, 5),    # Numeric
    col2 = factor(c("A", "B", "C", "D", "E")), # Factor
    col3 = c(10, 20, 0, 40, 50) # Numeric
  )

  result <- descriptive_stats_col(mixed_data)

  # Check if only numeric columns are processed
  expect_equal(nrow(result), 2)  # Only col1 and col3 are numeric

  # Check if zeros were correctly converted to NA
  expect_equal(result["col1", "Missing"], 1)  # One zero converted to NA in col1
  expect_equal(result["col3", "Missing"], 1)  # One zero converted to NA in col3

  # Check the mean values ignoring NAs
  expect_equal(result["col1", "Mean"], mean(c(1, 2, 4, 5)))
  expect_equal(result["col3", "Mean"], mean(c(10, 20, 40, 50)))
})
########################################test6########################################
test_that("descriptive_stats_col throws an error for non-data-frame input", {
  expect_error(descriptive_stats_col(123), "Input must be a data frame.")
  expect_error(descriptive_stats_col("test"), "Input must be a data frame.")
  expect_error(descriptive_stats_col(list(a = 1, b = 2)), "Input must be a data frame.")
})
########################################test7########################################
test_that("descriptive_stats_col handles empty data frames", {
  empty_data <- data.frame()
  result <- descriptive_stats_col(empty_data)

  # The result should be an empty data frame
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})
