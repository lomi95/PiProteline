test_that("convert_ID function works correctly", {
  # Create a mock dataset
  dataset <- data.frame(GeneName = c("BRCA1", "TP53", "EGFR","gewrgv"),
                        Value = c(5.6, 3.8, 7.2,5))

  # Mock the rbioapi function using the mockery package
  mock_rba_string_map_ids <- mockery::mock(data.frame(
    queryItem = c("BRCA1", "TP53"),
    preferredName = c("BRCA1_HUMAN", "TP53_HUMAN")
  ))

  # Temporarily replace the real function with the mock
  mockery::stub(convert_ID, "rbioapi::rba_string_map_ids", mock_rba_string_map_ids)

  # Test the function with the mock data
  result <- convert_ID(dataset, gene_column = 1, tax_ID = 9606)

  # Verify the output
  expect_equal(result$GeneName, c("BRCA1_HUMAN", "TP53_HUMAN", "EGFR", "gewrgv"))

  # Expect a message about the unmapped gene
  mock_rba_string_map_ids <- mockery::mock(data.frame(
    queryItem = c("BRCA1", "TP53"),
    preferredName = c("BRCA1_HUMAN", "TP53_HUMAN")
  ))
  mockery::stub(convert_ID, "rbioapi::rba_string_map_ids", mock_rba_string_map_ids)
  expect_message(convert_ID(dataset, gene_column = 1, tax_ID = 9606),
                 "EGFR, gewrgv was/were not mapped to Gene Name")
})
