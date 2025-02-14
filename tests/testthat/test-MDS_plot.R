test_that("MDS_plot generates the correct plot with default parameters", {
  # Create mock t_dataset
  t_dataset <- matrix(rnorm(100), nrow = 10)
  rownames(t_dataset) <- c("control_1", "control_2", "treatment_1", "treatment_2", "control_3",
                         "treatment_3", "control_4", "control_5", "treatment_4", "treatment_5")

  # Generate MDS plot
  plot <- MDS_plot(t_dataset, names_of_groups = c("control", "treatment"))

  # Check that the result is a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("MDS_plot handles pos_vectors_groups correctly", {
  # Create mock t_dataset
  t_dataset <- matrix(rnorm(100), nrow = 10)
  rownames(t_dataset) <- paste0("sample_", 1:10)

  # Define pos_vectors_groups
  pos_vectors_groups <- list(Group1 = c(1, 3, 5), Group2 = c(2, 4, 6))

  # Generate MDS plot
  plot <- MDS_plot(t_dataset, pos_vectors_groups = pos_vectors_groups)

  # Check that the result is a ggplot object
  expect_s3_class(plot, "ggplot")
})

test_that("MDS_plot stops when pos_vectors_groups is not a list", {
  t_dataset <- matrix(rnorm(100), nrow = 10)
  rownames(t_dataset) <- paste0("sample_", 1:10)

  expect_error(MDS_plot(t_dataset, pos_vectors_groups = c(1, 2, 3)),
               "'pos_vectors_groups' should be a list")
})

test_that("MDS_plot warns about exceeding nchar limit", {
  # Create mock t_dataset with long column names
  t_dataset <- matrix(rnorm(100), nrow = 20)
  colnames(t_dataset) <- c("veryveryvery_long_name_1", "veryveryveryveryvery_long_name_2",
                         "short_name", "another_veryveryvery_long_name","short_name2")
  rownames(t_dataset) <- paste0("sample_", 1:20)

  # Expect warning when column names exceed the character limit
  expect_message(MDS_plot(t_dataset, pos_vectors_groups = list(1:10,11:20)),
                 paste(paste(colnames(t_dataset)[nchar(colnames(t_dataset)) >= 15], collapse = ", "),
                 "is/are exceeding the nchar limit of 15, it/they will be truncated. Consider changing these names in the t_dataset"))
})
