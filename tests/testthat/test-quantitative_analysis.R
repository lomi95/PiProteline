set.seed(1)
test_that("quantitative_analysis performs LDA correctly", {
  data <- data.frame(
    matrix(runif(1500),ncol = 15,
           dimnames = list(paste0("Gene_",1:100),
                           paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
  )
  data$Gene <- paste0("Gene_",1:100)
  result <- quantitative_analysis(dataset = data,
                                  names_of_groups = c("Group1", "Group2", "Group3"),
                                  gene_column = ncol(data),
                                  significance.LDA = 0.05)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the LDA results are present
  expect_true(!is.null(result$LDA_results))

  # Check if the pairwise LDA results are calculated for more than two groups
  expect_true(!is.null(result$LDA_pairw.results))
})

test_that("quantitative_analysis handles two groups without pairwise LDA", {
  data <- data.frame(
    matrix(runif(1500),ncol = 15,
           dimnames = list(paste0("Gene_",1:100),
                           paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
  )
  data$Gene <- paste0("Gene_",1:100)

  result <- quantitative_analysis(data, names_of_groups = c("Group1", "Group2"),
                                  gene_column = ncol(data),
                                  significance.LDA = 0.05)

  # Check if the output is a list
  expect_type(result, "list")

  # Verify that the LDA results are present
  expect_true("LDA_results" %in% names(result))

  # Check if the pairwise LDA results are NULL for two groups
  expect_null(result$LDA_results)
})

test_that("quantitative_analysis calculates additional indices correctly", {
  data <- data.frame(
    matrix(runif(1500),ncol = 15,
           dimnames = list(paste0("Gene_",1:100),
                           paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
  )
  data$Gene <- paste0("Gene_",1:100)

  result <- quantitative_analysis(data, names_of_groups = c("Group1", "Group2", "Group3"),
                                  gene_column = ncol(data), significance.LDA = 0.05)

  # Check if the additional indices are not NULL
  expect_true(!is.null(result$DAve_index))
  expect_true(!is.null(result$DCI_index))
  expect_true(!is.null(result$Fold_Change))
})


test_that("quantitative_analysis handles different argument lists for LDA functions", {
  data <- data.frame(
    matrix(runif(1500),ncol = 15,
           dimnames = list(paste0("Gene_",1:100),
                           paste0("Group",rep(c(1,2,3),each = 5),"_",c(1:5))))
  )
  data$Gene <- paste0("Gene_",1:100)


  # Pass additional arguments to LDA function
  result <- quantitative_analysis(data, names_of_groups = c("Group1", "Group2", "Group3"),
                                  gene_column = ncol(data), significance.LDA = 0.05,
                                  someLDAParameter = TRUE)

  # Check if the LDA results are not NULL
  expect_true(!is.null(result$LDA_results))
})
