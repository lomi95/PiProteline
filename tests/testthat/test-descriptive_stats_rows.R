########################################test1########################################
test_that("descriptive_stats_rows works with numeric data", {
  test_data <- data.frame(
    col1 = c(1, 2, 0),
    col2 = c(10, 20, 0),
    col3 = c(5, 0, 15)
  )

  result <- suppressMessages(descriptive_stats_rows(test_data))

  # Check the structure of the output
  expect_true(is.data.frame(result))

  # Check if the correct number of rows is returned
  expect_equal(nrow(result), nrow(test_data))

  # Check the output for specific values
  expect_equal(result$Mean[1], mean(c(1, 10, 5), na.rm = TRUE))  # First row
  expect_equal(result$Mean[2], mean(c(2, 20), na.rm = TRUE))     # Zero is NA, so mean of 2 and 20
  expect_equal(result$Mean[3], mean(c(15), na.rm = TRUE))        # Only one valid value in the third row
})
########################################test2########################################
test_that("descriptive_stats_rows handles non-numeric columns", {
  test_data <- data.frame(
    col1 = c(1, 2, 0),
    col2 = c(10, 20, 0),
    col3 = c("A", "B", "C")  # Non-numeric
  )

  result <- suppressMessages(descriptive_stats_rows(test_data))

  # Ensure the function only processes the numeric columns
  expect_equal(nrow(result), nrow(test_data))

  # Check specific row calculations
  expect_equal(result$Mean[1], mean(c(1, 10), na.rm = TRUE))
  expect_equal(result$Mean[2], mean(c(2, 20), na.rm = TRUE))

  # When all numeric columns in a row are NA, the mean should be NA
  expect_true(is.na(result$Mean[3]))
})
########################################test3########################################
test_that("descriptive_stats_rows handles rows with only zeros", {
  test_data <- data.frame(
    col1 = c(0, 0, 0),
    col2 = c(0, 0, 0)
  )

  result <- suppressMessages(descriptive_stats_rows(test_data))

  # Check if all values are considered missing (NA)
  expect_equal(result$Missing[1], 2) # Both columns are zeros (converted to NA)
  expect_equal(result$Missing[2], 2)

  # Check if mean, min, and max return NA since all values are NA
  expect_true(is.na(result$Mean[1]))
  expect_true(is.na(result$Min[1]))
  expect_true(is.na(result$Max[1]))
})
test_that("descriptive_stats_rows handles mixed data types", {
  test_data <- data.frame(
    col1 = c(1, 2, 0),
    col2 = c(10, 20, 0),
    col3 = factor(c("A", "B", "C"))
  )

  result <- suppressMessages(descriptive_stats_rows(test_data))

  # Check if the function only calculates stats for numeric columns
  expect_equal(nrow(result), nrow(test_data))

  # Verify row-wise calculations for numeric columns only
  expect_equal(result$Mean[1], mean(c(1, 10), na.rm = TRUE))
  expect_equal(result$Mean[2], mean(c(2, 20), na.rm = TRUE))
})
########################################test5########################################
test_that("descriptive_stats_rows throws an error for non-data-frame input", {
  expect_error(descriptive_stats_rows(123), "Input must be a data frame.")
  expect_error(descriptive_stats_rows("test"), "Input must be a data frame.")
  expect_error(descriptive_stats_rows(list(a = 1, b = 2)), "Input must be a data frame.")
})
########################################test6########################################
test_that("descriptive_stats_rows handles empty data frames", {
  empty_data <- data.frame()
  expect_warning(descriptive_stats_rows(empty_data),"Input data frame is empty. No statistics to compute.")
})
