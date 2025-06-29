test_that("calculate_is_score works correctly", {
  # Species as rownames, samples as columns
  test_otu <- data.frame(
    Sample1 = c(0.2, 0.3),
    Sample2 = c(0.1, 0),
    row.names = c("Species1", "Species2")
  )

  test_binding <- data.frame(
    Sample1 = c(1.5, 1.0),
    Sample2 = c(1.2, 1.3),
    row.names = c("Species1", "Species2")
  )

  result <- calculate_is_score(test_otu, test_binding)

  expect_true(is.data.frame(result))
})


test_that("combine_and_zscore correctly computes z-scores", {
  df1 <- data.frame(IS_Global_Score = c(0.5, 0.6), row.names = c("Sample1", "Sample2"))
  df2 <- data.frame(IS_Global_Score = c(0.7, 0.8), row.names = c("Sample3", "Sample4"))

  result <- calculate_isgm_index(df1, df2)

  expect_true("Z_score" %in% colnames(result))
  expect_equal(nrow(result), 4)
  expect_equal(round(mean(result$Z_score), 10), 0)  # Z-score mean is ~0
})
