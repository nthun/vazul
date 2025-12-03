# Tests for mask_variables_rowwise function

test_that("mask_variables_rowwise works with single variable set", {
  df <- data.frame(
    treat_1 = c("control", "treatment", "placebo"),
    treat_2 = c("treatment", "placebo", "control"),
    treat_3 = c("placebo", "control", "treatment"),
    other = c("X", "Y", "Z")
  )

  set.seed(123)
  result <- expect_warning(
    mask_variables_rowwise(df, c("treat_1", "treat_2", "treat_3")),
    NA  # expect NO warning
  )

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(df))
  expect_equal(names(result), names(df))

  # Check that masking was applied to selected columns
  expect_true(all(grepl("^masked_group_", result$treat_1)))
  expect_true(all(grepl("^masked_group_", result$treat_2)))
  expect_true(all(grepl("^masked_group_", result$treat_3)))

  # Check that non-selected columns remained unchanged
  expect_equal(result$other, df$other)
})

test_that("mask_variables_rowwise works with multiple variable sets", {
  df <- data.frame(
    treat_1 = c("control", "treatment", "placebo"),
    treat_2 = c("treatment", "placebo", "control"),
    cond_a = c("A", "B", "A"),
    cond_b = c("B", "A", "B"),
    id = 1:3
  )

  set.seed(456)
  result <- expect_warning(
    mask_variables_rowwise(df, c("treat_1", "treat_2"), c("cond_a", "cond_b")),
    NA
  )

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(df))

  # Treatment columns should be masked
  expect_true(all(grepl("^masked_group_", result$treat_1)))
  expect_true(all(grepl("^masked_group_", result$treat_2)))

  # Condition columns should be masked
  expect_true(all(grepl("^masked_group_", result$cond_a)))
  expect_true(all(grepl("^masked_group_", result$cond_b)))

  # ID column should remain unchanged
  expect_equal(result$id, df$id)
})

test_that("mask_variables_rowwise works with tidyselect expressions", {
  df <- data.frame(
    treat_1 = c("A", "B", "C"),
    treat_2 = c("B", "C", "A"),
    other_1 = c("X", "Y", "Z"),
    other_2 = c("Y", "Z", "X"),
    id = 1:3
  )

  set.seed(789)
  result <- expect_warning(
    mask_variables_rowwise(df, dplyr::starts_with("treat_")),
    NA
  )

  expect_s3_class(result, "data.frame")

  # Only treat_ columns should be masked
  expect_true(all(grepl("^masked_group_", result$treat_1)))
  expect_true(all(grepl("^masked_group_", result$treat_2)))

  # Other columns should remain unchanged
  expect_equal(result$other_1, df$other_1)
  expect_equal(result$other_2, df$other_2)
  expect_equal(result$id, df$id)
})

test_that("mask_variables_rowwise maintains consistent mapping within sets", {
  df <- data.frame(
    x = c("A", "B", "A"),
    y = c("B", "A", "C"),
    z = c("A", "C", "B")
  )

  set.seed(111)
  result <- mask_variables_rowwise(df, c("x", "y", "z"))

  # Same values should get same masked labels within the set
  all_orig <- unlist(df[c("x", "y", "z")])
  all_masked <- unlist(result[c("x", "y", "z")])

  # All "A"s should have same mask
  a_positions <- which(all_orig == "A")
  a_masked <- all_masked[a_positions]
  expect_true(all(a_masked == a_masked[1]))

  # All "B"s should have same mask
  b_positions <- which(all_orig == "B")
  b_masked <- all_masked[b_positions]
  expect_true(all(b_masked == b_masked[1]))
})

test_that("mask_variables_rowwise handles factor columns", {
  df <- data.frame(
    x = factor(c("Low", "High", "Medium")),
    y = factor(c("High", "Low", "High"))
  )

  set.seed(222)
  result <- mask_variables_rowwise(df, c("x", "y"))

  expect_s3_class(result$x, "factor")
  expect_s3_class(result$y, "factor")
  expect_true(all(grepl("^masked_group_", as.character(result$x))))
  expect_true(all(grepl("^masked_group_", as.character(result$y))))
})

test_that("mask_variables_rowwise handles NA values", {
  df <- data.frame(
    x = c("A", NA, "C"),
    y = c(NA, "B", "A"),
    z = c("C", "A", NA)
  )

  set.seed(333)
  result <- mask_variables_rowwise(df, c("x", "y", "z"))

  # NA values should remain NA
  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$y[1]))
  expect_true(is.na(result$z[3]))

  # Non-NA values should be masked
  expect_true(grepl("^masked_group_", result$x[1]))
  expect_true(grepl("^masked_group_", result$y[2]))
  expect_true(grepl("^masked_group_", result$z[1]))
})

test_that("mask_variables_rowwise uses custom prefix", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c("B", "C", "A")
  )

  set.seed(444)
  result <- mask_variables_rowwise(df, c("x", "y"), prefix = "custom_")

  expect_true(all(grepl("^custom_", result$x)))
  expect_true(all(grepl("^custom_", result$y)))
  expect_false(any(grepl("^masked_group_", result$x)))
})

test_that("mask_variables_rowwise validates input correctly", {
  df <- data.frame(x = c("A", "B", "C"), y = c("X", "Y", "Z"))

  # Non-data.frame input
  expect_error(
    mask_variables_rowwise(list(x = c("A", "B")), "x"),
    "Input 'data' must be a data frame.",
    fixed = TRUE
  )

  # No column sets provided
  expect_warning(
    result <- mask_variables_rowwise(df),
    "No column sets provided"
  )
  expect_equal(result, df)

  # Nonexistent columns - now shows warning about missing columns
  expect_warning(
    mask_variables_rowwise(df, "nonexistent_column"),
    "Each column set must be a character vector or tidyselect expression."
  )

  # Invalid column set type - shows warning from failed evaluation
  expect_warning(
    mask_variables_rowwise(df, data.frame(a = 1)),
    "Failed to evaluate column set"
  )
})

test_that("mask_variables_rowwise handles mixed data types correctly", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c(1, 2, 3),  # numeric
    z = c("X", "Y", "Z")
  )

  # Should throw error when trying to mask non-character/factor columns
  # mask_variables_rowwise throws an error for non-categorical columns
  expect_error(
    mask_variables_rowwise(df, c("x", "y", "z")),
    "All selected columns must be character or factor"
  )
})

test_that("mask_variables_rowwise handles edge cases", {
  # Single column - mask_variables_rowwise does not warn, it just masks the single column
  df_single_col <- data.frame(x = c("A", "B", "C"))
  result <- mask_variables_rowwise(df_single_col, "x")
  expect_true(all(grepl("^masked_group_", result$x)))

  # Single row
  df_single_row <- data.frame(x = "A", y = "B")
  result <- mask_variables_rowwise(df_single_row, c("x", "y"))
  expect_equal(nrow(result), 1)
  expect_true(all(grepl("^masked_group_", unlist(result[c("x", "y")]))))

  # Empty character vector should not produce a warning but return original data
  df_empty <- data.frame(x = c("A", "B"), y = c("X", "Y"))
  expect_warning({result <- mask_variables_rowwise(df_empty)})
  expect_equal(result, df_empty)
})
