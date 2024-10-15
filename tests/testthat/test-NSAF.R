test_that("NSAF calculates the correct index for a single dataset", {
  length_proteins <- c(300, 500, 200)
  samples <- matrix(
    c(10, 20, 30,
      40, 50, 60,
      70, 80, 90),
    nrow = 3, byrow = TRUE
  )

  result <- NSAF(samples, length_proteins)

  # Check if the result is a numeric vector
  expect_type(result, "double")

  # Check if the length matches the number of proteins
  expect_equal(length(result), length(length_proteins))
})

test_that("NSAF calculates the correct index for a list of groups", {
  length_proteins <- c(300, 500, 200)
  group1 <- matrix(
    c(5, 10, 15,
      20, 25, 30),
    nrow = 2, byrow = TRUE
  )

  group2 <- matrix(
    c(8, 16, 24,
      32, 40, 48),
    nrow = 2, byrow = TRUE
  )

  result <- NSAF(list(group1, group2), length_proteins)

  # Check if the result is a matrix
  expect_true(is.matrix(result))

  # Check if the number of rows matches the number of proteins
  expect_equal(nrow(result), length(length_proteins))

  # Check if the number of columns matches the number of groups
  expect_equal(ncol(result), 2)
})

test_that("NSAF works with different numbers of samples and proteins", {
  length_proteins <- c(300, 500, 200)
  samples <- matrix(
    c(10, 20, 30,
      40, 50, 60),
    nrow = 2, byrow = TRUE
  )

  result <- NSAF(samples, length_proteins)

  # Check if the result is a numeric vector
  expect_type(result, "double")

  # Check if the length matches the number of proteins
  expect_equal(length(result), length(length_proteins))
})
