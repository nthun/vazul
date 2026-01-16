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
    c("treat_1", "treat_2", "outcome_a", "outcome_b"),
    prefix = "var_"
  )

  # Check that data frame has same number of columns
  expect_equal(ncol(result), ncol(df))
  expect_equal(nrow(result), nrow(df))

  # Check that the id column is unchanged
  expect_equal(result$id, df$id)

  # Check that masked names follow expected pattern
  masked_names <- names(result)
  var_names <- masked_names[grepl("^var_", masked_names)]

  expect_equal(length(var_names), 4)  # All 4 selected columns
  expect_true("id" %in% masked_names)  # unchanged column
  expect_true(all(grepl("^var_\\d{2}$", var_names)))  # var_01, var_02, etc.
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
    prefix = "treatment_"
  )

  # Check masked names exist
  masked_names <- names(result)
  expect_true(all(grepl("^treatment_\\d{2}$", masked_names[1:2])))
  expect_true("id" %in% masked_names)
  expect_equal(sum(grepl("^treatment_", masked_names)), 2)
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
  expect_true(all(grepl("^masked_var_\\d{2}$", masked_names)))
  expect_equal(length(masked_names), 2)
})

test_that("mask_names detects name collisions", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    var_01 = c(7, 8, 9)  # This will cause collision
  )

  set.seed(123)
  expect_error(
    mask_names(df, c("var1", "var2"), prefix = "var_"),
    "Name collision detected"
  )
})

test_that("mask_names handles empty column sets gracefully", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )

  expect_warning(
    result <- mask_names(df, prefix = "var_"),
    "No columns selected"
  )
  expect_equal(result, df)
})

test_that("mask_names validates parameters correctly", {
  df <- data.frame(var1 = c(1, 2, 3))

  # Test missing prefix
  expect_error(
    mask_names(df, c("var1")),
    "Parameter 'prefix' is required"
  )

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

  # Test empty string prefix
  expect_error(
    mask_names(df, c("var1"), prefix = ""),
    "Parameter 'prefix' cannot be an empty string"
  )
})

test_that("mask_names warns about prefix collisions", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    masked_other = c(7, 8, 9)  # Shares prefix
  )

  set.seed(123)
  expect_warning(
    result <- mask_names(df, c("var1", "var2"), prefix = "masked_"),
    "Masked names use prefix 'masked_' which matches existing column"
  )
})

test_that("mask_names preserves data content while changing names", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )

  set.seed(123)
  result <- mask_names(df, c("var1", "var2"), prefix = "v_")

  # Check that data content is preserved
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  
  # Check that values are unchanged (just names changed)
  expect_equal(unname(sort(unlist(result))), unname(sort(unlist(df))))
})

test_that("mask_names produces consistent results with set.seed", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    var3 = c(7, 8, 9)
  )

  set.seed(42)
  result1 <- mask_names(df, c("var1", "var2", "var3"), prefix = "x_")

  set.seed(42)
  result2 <- mask_names(df, c("var1", "var2", "var3"), prefix = "x_")

  expect_equal(names(result1), names(result2))
})

test_that("mask_names works with single variable", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )

  set.seed(123)
  result <- mask_names(df, c("var1"), prefix = "masked_")

  expect_equal(ncol(result), 2)
  expect_true(any(grepl("^masked_01$", names(result))))
  expect_true("var2" %in% names(result))  # unchanged
})

test_that("mask_names handles data frame input validation", {
  # Test non-data.frame input
  expect_error(
    mask_names("not_a_dataframe", c("var1"), prefix = "x_"),
    "Input 'data' must be a data frame"
  )

  # Test empty data frame
  expect_error(
    mask_names(data.frame(), c("var1"), prefix = "x_"),
    "Input 'data' cannot be an empty data frame"
  )
})

test_that("mask_names works with multiple tidyselect calls", {
  df <- data.frame(
    treat_1 = c(1, 2, 3),
    treat_2 = c(4, 5, 6),
    outcome_a = c(7, 8, 9),
    outcome_b = c(10, 11, 12),
    id = 1:3
  )

  set.seed(123)
  # Mask treatment variables first
  result <- df |>
    mask_names(starts_with("treat_"), prefix = "T_") |>
    mask_names(starts_with("outcome_"), prefix = "O_")

  masked_names <- names(result)
  expect_equal(sum(grepl("^T_", masked_names)), 2)
  expect_equal(sum(grepl("^O_", masked_names)), 2)
  expect_true("id" %in% masked_names)
})

test_that("mask_names randomizes name assignment", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    var3 = c(7, 8, 9)
  )

  set.seed(123)
  result1 <- mask_names(df, c("var1", "var2", "var3"), prefix = "x_")

  set.seed(456)
  result2 <- mask_names(df, c("var1", "var2", "var3"), prefix = "x_")

  # Names should be different with different seeds
  expect_false(identical(names(result1), names(result2)))
})
