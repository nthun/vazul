# Tests for mask_names function

test_that("mask_names basic functionality works", {
  df <- data.frame(
    treat_1 = c(1, 2, 3),
    treat_2 = c(4, 5, 6),
    outcome_a = c(7, 8, 9),
    outcome_b = c(10, 11, 12),
    id = 1:3,
    stringsAsFactors = FALSE
  )

  set.seed(123)
  result <- mask_names(df,
    c("treat_1", "treat_2"),
    c("outcome_a", "outcome_b")
  )

  # Check that data frame has same number of columns
  expect_equal(ncol(result), ncol(df))
  expect_equal(nrow(result), nrow(df))

  # Check that the id column is unchanged
  expect_equal(result$id, df$id)

  # Check that masked names follow expected pattern
  masked_names <- names(result)
  treat_names <- masked_names[grepl("^variable_set_A_", masked_names)]
  outcome_names <- masked_names[grepl("^variable_set_B_", masked_names)]

  expect_equal(length(treat_names), 2)  # treat_1, treat_2
  expect_equal(length(outcome_names), 2)  # outcome_a, outcome_b
  expect_true("id" %in% masked_names)  # unchanged column
})

test_that("mask_names works with tidyselect helpers", {
  df <- data.frame(
    treat_1 = c(1, 2, 3),
    treat_2 = c(4, 5, 6),
    outcome_a = c(7, 8, 9),
    outcome_b = c(10, 11, 12),
    id = 1:3
  )

  set.seed(123)
  result <- mask_names(df,
    starts_with("treat_"),
    starts_with("outcome_")
  )

  # Check masked names exist
  masked_names <- names(result)
  expect_true(any(grepl("^variable_set_A_", masked_names)))
  expect_true(any(grepl("^variable_set_B_", masked_names)))
  expect_true("id" %in% masked_names)
})

test_that("mask_names custom prefix works", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )

  set.seed(123)
  result <- mask_names(df,
    c("var1", "var2"),
    prefix = "masked_var_"
  )

  masked_names <- names(result)
  expect_true(all(grepl("^masked_var_A_", masked_names)))
})

test_that("mask_names custom set_id works", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    var3 = c(7, 8, 9)
  )

  set.seed(123)
  result <- mask_names(df,
    c("var1", "var2"),
    c("var3"),
    set_id = c("group1", "group2")
  )

  masked_names <- names(result)
  expect_true(any(grepl("^variable_set_group1_", masked_names)))
  expect_true(any(grepl("^variable_set_group2_", masked_names)))
})

test_that("mask_names detects name collisions", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    variable_set_A_01 = c(7, 8, 9)  # This will cause collision
  )

  set.seed(123)
  expect_error(
    mask_names(df, c("var1", "var2")),
    "Name collision detected"
  )
})

test_that("mask_names handles empty column sets gracefully", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )

  expect_warning(
    result <- mask_names(df),
    "No variable sets provided"
  )
  expect_equal(result, df)
})

test_that("mask_names handles non-existent columns", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )

    expect_warning(
      result <- mask_names(df, c("var1", "nonexistent")),
      "Some column names not found")
})

test_that("mask_names validates parameters correctly", {
  df <- data.frame(var1 = c(1, 2, 3))

  # Test NULL prefix
  expect_error(
    mask_names(df, c("var1"), prefix = NULL),
    "Parameter 'prefix' cannot be NULL"
  )

  # Test non-character prefix
  expect_error(
    mask_names(df, c("var1"), prefix = 123),
    "Parameter 'prefix' must be a single character string"
  )

  # Test multiple string prefix
  expect_error(
    mask_names(df, c("var1"), prefix = c("a", "b")),
    "Parameter 'prefix' must be a single character string"
  )

  # Test wrong length set_id
  expect_error(
    mask_names(df, c("var1"), c("var1"), set_id = c("A")),  # 2 sets, 1 set_id
    "If 'set_id' is provided, it must have the same length as the number of variable sets"
  )
})

test_that("mask_names prevents duplicate masked names", {
  # Create a scenario where the same column name exists in multiple sets
  df <- data.frame(
    common = c(1, 2, 3),
    var1 = c(4, 5, 6),
    var2 = c(7, 8, 9)
  )

  # This should work fine with different set_ids
  set.seed(123)
  result <- mask_names(df,
    c("common", "var1"),
    c("var2"),
    set_id = c("set1", "set2")
  )

  expect_equal(ncol(result), ncol(df))
  expect_equal(nrow(result), nrow(df))

  expect_error(mask_names(df, c("common"), c("var1"), set_id = c("A", "A")))
  expect_error(mask_names(df, c("common", "var1"), c("common", "var2")))

})

test_that("mask_names preserves data content while changing names", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    unchanged = c(7, 8, 9)
  )

  set.seed(123)
  result <- mask_names(df, c("var1", "var2"))

  # Check that data content is preserved
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))

  # Unchanged column should be identical
  expect_equal(result$unchanged, df$unchanged)

  # Masked columns should have same data but different names
  masked_cols <- names(result)[grepl("^variable_set_", names(result))]
  expect_equal(length(masked_cols), 2)

  # The sum of values should be preserved (simple check)
  expect_equal(sum(unlist(result)), sum(unlist(df)))
})

test_that("mask_names works with single variable set", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    unchanged = c(7, 8, 9)
  )

  set.seed(123)
  result <- mask_names(df, c("var1", "var2"))

  masked_names <- names(result)
  expect_true(any(grepl("^variable_set_A_", masked_names)))
  expect_true("unchanged" %in% masked_names)
  expect_equal(length(masked_names), 3)
})

test_that("mask_names handles data frame input validation", {
  # Test non-data.frame input
  expect_error(
    mask_names("not_a_dataframe", c("var1")),
    "is.data.frame\\(data\\) is not TRUE"
  )

  # Test with matrix
  mat <- matrix(1:6, ncol = 2)
  expect_error(
    mask_names(mat, c("V1")),
    "is.data.frame\\(data\\) is not TRUE"
  )
})
