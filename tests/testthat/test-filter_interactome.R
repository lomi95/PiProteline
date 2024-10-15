test_that("filter_interactome works correctly", {
  library(dplyr)

  # example dataset
  interactome.hs <- data.frame(
    protein1 = c("P12345", "P67890", "P23456", "P34567"),
    protein2 = c("P54321", "P09876", "P65432", "P76543"),
    experimental_score = c(200, 120, 180, 160),
    database_score = c(400, 300, 350, 320)
  )

  # Test 1: Filtering with standard thresholds
  score_thresholds <- c("experimental_score" = 150, "database_score" = 350)
  filtered <- filter_interactome(interactome.hs, score_thresholds)

  expect_equal(nrow(filtered), 3)
  expect_true(all(filtered$experimental_score > 150 | filtered$database_score > 350))

  # Test 2: Threshold over the ranges
  score_thresholds_low <- c("experimental_score" = 300, "database_score" = 400)
  filtered_low <- filter_interactome(interactome.hs, score_thresholds_low)

  expect_equal(nrow(filtered_low), 0)  #

  # Test 3: Error check if names don't correspond
  score_thresholds_wrong <- c("wrong_column" = 150)
  expect_error(filter_interactome(interactome.hs, score_thresholds_wrong),
               "The names of 'scores_threshold' do not match the column names of the interactome")

  # Test 4: Check NA handling
  interactome_with_na <- data.frame(
    protein1 = c("P12345", "P67890", "P23456", "P34567"),
    protein2 = c("P54321", "P09876", "P65432", "P76543"),
    experimental_score = c(200, NA, 180, 160),
    database_score = c(400, 300, NA, 320)
  )
  filtered_na <- filter_interactome(interactome_with_na, score_thresholds)

  expect_equal(nrow(filtered_na), 3)
})
