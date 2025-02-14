test_that("quantitative_analysis performs manova correctly", {
  set.seed(1)
  data <- data.frame(
    matrix(runif(1500),ncol = 15,
           dimnames = list(paste0("Gene_",1:100),
                           paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
  )
  data$Gene <- paste0("Gene_",1:100)
  result <- quantitative_analysis(dataset = data,
                                  names_of_groups = c("Group1", "Group2", "Group3"),
                                  gene_column = ncol(data),
                                  significance_manova = 0.05)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the manova results are present
  expect_true(!is.null(result$manova_results))

  # Check if the pairwise manova results are calculated for more than two groups
  expect_true(!is.null(result$manova_pairw_results))
})

test_that("quantitative_analysis handles two groups without pairwise manova", {
  set.seed(1)
  data <- data.frame(
    matrix(runif(1500),ncol = 15,
           dimnames = list(paste0("Gene_",1:100),
                           paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
  )
  data$Gene <- paste0("Gene_",1:100)

  result <- quantitative_analysis(data, names_of_groups = c("Group1", "Group2"),
                                  gene_column = ncol(data),
                                  significance_manova = 0.05)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the manova results are not present
  expect_true(!"manova_results" %in% names(result))

  # Check if the pairwise manova results are NULL for two groups
  expect_null(result$manova_results)
})



test_that("quantitative_analysis handles different argument lists for manova functions", {
  set.seed(1)
  data <- data.frame(
    matrix(runif(1500),ncol = 15,
           dimnames = list(paste0("Gene_",1:100),
                           paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
  )
  data$Gene <- paste0("Gene_",1:100)


  # Pass additional arguments to manova function
  result <- quantitative_analysis(data, names_of_groups = c("Group1", "Group2", "Group3"),
                                  gene_column = ncol(data), significance_manova = 0.05,
                                  somemanovaParameter = TRUE)

  # Check if the manova results are not NULL
  expect_true(!is.null(result$manova_results))
})
