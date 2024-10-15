test_that("MDS_plot generates the correct plot with default parameters", {
  # Create mock dataset
  dataset <- matrix(rnorm(100), nrow = 10)
  rownames(dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3",
                         "treatment_3", "control_4", "control_5", "treatment_4", "treatment_5")

  # Generate MDS plot
  plot <- MDS_plot(dataset, names_of_groups = c("control", "treatment"))

  # Check that the result is a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("MDS_plot handles pos.vectors_groups correctly", {
  # Create mock dataset
  dataset <- matrix(rnorm(100), nrow = 10)
  rownames(dataset) <- paste0("sample_", 1:10)

  # Define pos.vectors_groups
  pos.vectors_groups <- list(Group1 = c(1, 3, 5), Group2 = c(2, 4, 6))

  # Generate MDS plot
  plot <- MDS_plot(dataset, pos.vectors_groups = pos.vectors_groups)

  # Check that the result is a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("MDS_plot stops when pos.vectors_groups is not a list", {
  dataset <- matrix(rnorm(100), nrow = 10)
  rownames(dataset) <- paste0("sample_", 1:10)

  expect_error(MDS_plot(dataset, pos.vectors_groups = c(1, 2, 3)),
               "'pos.vectors_groups' should be a list")
})

test_that("MDS_plot warns about exceeding nchar limit", {
  # Create mock dataset with long column names
  dataset <- matrix(rnorm(100), nrow = 20)
  colnames(dataset) <- c("veryveryvery_long_name_1", "veryveryveryveryvery_long_name_2",
                         "short_name", "another_veryveryvery_long_name","short_name2")
  rownames(dataset) <- paste0("sample_", 1:20)

  # Expect warning when column names exceed the character limit
  expect_message(MDS_plot(dataset, pos.vectors_groups = list(1:10,11:20)),
                 paste(paste(colnames(dataset)[nchar(colnames(dataset)) >= 15], collapse = ", "),
                 "is/are exceeding the nchar limit of 15, it/they will be removed.
                 Consider changing these names in the dataset"))
})
