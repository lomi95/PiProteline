test_that("group_listing works with names_of_groups", {
  dataset <- matrix(rnorm(100), nrow = 10)
  rownames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3",
                         "treatment_3", "control_4", "control_5", "treatment_4", "treatment_5")

  result <- group_listing(dataset, names_of_groups = c("control", "treatment"))

  expect_equal(length(result), 2)
  expect_equal(nrow(result[[1]]), 5)  # Control group
  expect_equal(nrow(result[[2]]), 5)  # Treatment group
})

test_that("group_listing works with pos.vectors_groups", {
  dataset <- matrix(rnorm(100), nrow = 10)
  rownames(dataset) <- paste0("row", 1:10)

  result <- group_listing(dataset, pos.vectors_groups = list(c(1, 3, 5), c(2, 4, 6)))

  expect_equal(length(result), 2)
  expect_equal(nrow(result[[1]]), 3)
  expect_equal(nrow(result[[2]]), 3)
})

test_that("group_listing gives error with no groups or just one group", {
  dataset <- matrix(rnorm(100), nrow = 10)
  rownames(dataset) <- paste0("row", 1:10)

  expect_error(group_listing(dataset),
               "Both `names_of_groups` and `pos.vectors_groups` are missing, with no default"
  )
  expect_error(group_listing(dataset, pos.vectors_groups = list(c(1, 11))),
               "`pos.vectors_groups` should be a list of length at least 2"
  )
  expect_error(group_listing(dataset, names_of_groups = "Group1"),
               "`names_of_groups` should be a character string of length at least 2"
  )
})

test_that("group_listing handles out of bounds indices", {
  dataset <- matrix(rnorm(100), nrow = 10)
  rownames(dataset) <- paste0("row", 1:10)

  expect_warning(
    result <- group_listing(dataset, pos.vectors_groups = list(c(1, 11), c(2,4))),
    "One or more elements are out of dataset dimension limits"
  )
  expect_equal(nrow(result[[1]]), 1)
})

test_that("group_listing stops if pos.vectors_groups is not a list", {
  dataset <- matrix(rnorm(100), nrow = 10)

  expect_error(group_listing(dataset, pos.vectors_groups = c(1, 2, 3)),
               "'pos.vectors_groups' should be a list")
})
