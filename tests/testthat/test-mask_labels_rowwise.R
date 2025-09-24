# Tests for mask_labels_rowwise function

test_that("mask_labels_rowwise works with basic input", {
  df <- data.frame(
    treat_1 = c("control", "treatment", "placebo"),
    treat_2 = c("treatment", "placebo", "control"),
    other = c("X", "Y", "Z")
  )
  
  set.seed(123)
  result <- mask_labels_rowwise(df, c("treat_1", "treat_2"))
  
  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(df))
  expect_equal(names(result), names(df))
  
  # Check that masking was applied to selected columns
  expect_true(all(grepl("^masked_group_", result$treat_1)))
  expect_true(all(grepl("^masked_group_", result$treat_2)))
  
  # Check that non-selected columns remained unchanged
  expect_equal(result$other, df$other)
})

test_that("mask_labels_rowwise creates consistent mapping", {
  df <- data.frame(
    x = c("A", "B", "A"),
    y = c("B", "A", "C")
  )
  
  set.seed(456)
  result <- mask_labels_rowwise(df, c("x", "y"))
  
  # All "A"s should get the same masked label
  a_positions <- which(unlist(df[c("x", "y")]) == "A")
  a_masked_values <- unlist(result[c("x", "y")])[a_positions]
  expect_true(all(a_masked_values == a_masked_values[1]))
  
  # All "B"s should get the same masked label
  b_positions <- which(unlist(df[c("x", "y")]) == "B")
  b_masked_values <- unlist(result[c("x", "y")])[b_positions]
  expect_true(all(b_masked_values == b_masked_values[1]))
})

test_that("mask_labels_rowwise works with tidyselect", {
  df <- data.frame(
    treat_1 = c("A", "B", "C"),
    treat_2 = c("B", "C", "A"),
    other_1 = c("X", "Y", "Z")
  )
  
  set.seed(789)
  result <- mask_labels_rowwise(df, dplyr::starts_with("treat_"))
  
  expect_true(all(grepl("^masked_group_", result$treat_1)))
  expect_true(all(grepl("^masked_group_", result$treat_2)))
  expect_equal(result$other_1, df$other_1)
})

test_that("mask_labels_rowwise handles factors correctly", {
  df <- data.frame(
    x = factor(c("Low", "High", "Medium")),
    y = factor(c("High", "Low", "High"))
  )
  
  set.seed(111)
  result <- mask_labels_rowwise(df, c("x", "y"))
  
  expect_s3_class(result$x, "factor")
  expect_s3_class(result$y, "factor")
  
  # Should have 3 unique masked labels
  expect_equal(length(unique(c(as.character(result$x), as.character(result$y)))), 3)
})

test_that("mask_labels_rowwise handles NA values", {
  df <- data.frame(
    x = c("A", NA, "C"),
    y = c(NA, "B", "A")
  )
  
  set.seed(222)
  result <- mask_labels_rowwise(df, c("x", "y"))
  
  # NA values should remain NA
  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$y[1]))
  
  # Non-NA values should be masked
  expect_true(grepl("^masked_group_", result$x[1]))
  expect_true(grepl("^masked_group_", result$y[2]))
})

test_that("mask_labels_rowwise uses custom prefix", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c("B", "C", "A")
  )
  
  set.seed(333)
  result <- mask_labels_rowwise(df, c("x", "y"), prefix = "custom_")
  
  expect_true(all(grepl("^custom_", result$x)))
  expect_true(all(grepl("^custom_", result$y)))
  expect_false(any(grepl("^masked_group_", result$x)))
})

test_that("mask_labels_rowwise validates input correctly", {
  df <- data.frame(
    x = c("A", "B", "C"), 
    y = c(1, 2, 3),  # numeric
    z = c("X", "Y", "Z")
  )
  
  # Non-data.frame input
  expect_error(
    mask_labels_rowwise(list(x = c("A", "B")), "x"),
    class = "simpleError"
  )
  
  # No columns selected - this now throws an error from tidyselect
  expect_error(
    mask_labels_rowwise(df, "nonexistent"),
    "Can't subset columns that don't exist"
  )
  
  # Mixed data types
  expect_error(
    mask_labels_rowwise(df, c("x", "y")),
    "All selected columns must be character or factor"
  )
})

test_that("mask_labels_rowwise handles edge cases", {
  # Single column
  df <- data.frame(x = c("A", "B", "C"))
  result <- mask_labels_rowwise(df, "x")
  expect_true(all(grepl("^masked_group_", result$x)))
  
  # Single row
  df_single <- data.frame(x = "A", y = "B")
  result <- mask_labels_rowwise(df_single, c("x", "y"))
  expect_equal(nrow(result), 1)
  expect_true(all(grepl("^masked_group_", unlist(result[c("x", "y")]))))
  
  # All NA values - need character columns
  df_na <- data.frame(x = c(NA_character_, NA_character_), 
                      y = c(NA_character_, NA_character_))
  expect_warning(
    result <- mask_labels_rowwise(df_na, c("x", "y")),
    "No non-NA values found"
  )
  expect_equal(result, df_na)
})