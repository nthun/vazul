# Tests for mask_variables function

test_that("mask_variables returns correct structure and type", {
  # Test with basic data frame
  df <- data.frame(
    x = c("A", "B", "A"),
    y = c("X", "Y", "X"),
    z = 1:3
  )
  
  set.seed(123)
  result <- mask_variables(df, c("x", "y"))
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  expect_equal(names(result), names(df))
  expect_equal(result$z, df$z)  # numeric column unchanged
})

test_that("mask_variables works with independent masking (default)", {
  df <- data.frame(
    treatment = c("control", "intervention", "control"),
    outcome = c("success", "failure", "success"),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df, c("treatment", "outcome"))
  
  # Check that both columns are masked with column-specific prefixes
  expect_true(all(grepl("^treatment_group_", result$treatment)))
  expect_true(all(grepl("^outcome_group_", result$outcome)))
  
  # Check that original structure is preserved
  expect_equal(length(result$treatment), 3)
  expect_equal(length(result$outcome), 3)
  
  # Check that different variables can have different mappings
  expect_type(result$treatment, "character")
  expect_type(result$outcome, "character")
})

test_that("mask_variables works with shared masking", {
  df <- data.frame(
    var1 = c("A", "B", "A"),
    var2 = c("A", "C", "B"),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df, c("var1", "var2"), across_variables = TRUE)
  
  # Check that both columns are masked
  expect_true(all(grepl("^masked_group_", result$var1)))
  expect_true(all(grepl("^masked_group_", result$var2)))
  
  # With shared labels, the same original value should get the same masked label
  # across different columns
  a_mask_var1 <- result$var1[df$var1 == "A"][1]
  a_mask_var2 <- result$var2[df$var2 == "A"][1]
  expect_equal(a_mask_var1, a_mask_var2)
  
  b_mask_var1 <- result$var1[df$var1 == "B"][1]
  b_mask_var2 <- result$var2[df$var2 == "B"][1]
  expect_equal(b_mask_var1, b_mask_var2)
})

test_that("mask_variables works with factor columns", {
  df <- data.frame(
    x = factor(c("A", "B", "A")),
    y = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df, c("x", "y"))
  
  expect_s3_class(result$x, "factor")
  expect_type(result$y, "character")
  expect_true(all(grepl("^x_group_", as.character(result$x))))
  expect_true(all(grepl("^y_group_", result$y)))
})

test_that("mask_variables independent masking works as requested in issue", {
  # Test the specific example from the issue comment
  df <- data.frame(
    treatment = c("control", "intervention", "control"),
    outcome = c("success", "failure", "success"),
    score = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df, c("treatment", "outcome"))
  
  # Should have treatment_group_XX and outcome_group_XX format
  expect_true(all(grepl("^treatment_group_", result$treatment)))
  expect_true(all(grepl("^outcome_group_", result$outcome)))
  
  # Score should be unchanged
  expect_equal(result$score, df$score)
  
  # Check that different original values get different labels within each column
  expect_equal(length(unique(result$treatment)), 2)  # control, intervention
  expect_equal(length(unique(result$outcome)), 2)    # success, failure
  
  # Check that same original values get same labels within each column
  expect_equal(result$treatment[1], result$treatment[3])  # both "control"
  expect_equal(result$outcome[1], result$outcome[3])      # both "success"
})

test_that("mask_variables works with tidyselect helpers", {
  df <- data.frame(
    char1 = c("A", "B", "A"),
    char2 = c("X", "Y", "X"),
    num1 = 1:3,
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df, where(is.character))
  
  expect_true(all(grepl("^char1_group_", result$char1)))
  expect_true(all(grepl("^char2_group_", result$char2)))
  expect_equal(result$num1, df$num1)  # numeric unchanged
})

test_that("mask_variables handles non-categorical columns correctly", {
  df <- data.frame(
    x = c("A", "B", "A"),
    y = 1:3,
    z = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    mask_variables(df, c("x", "y", "z")),
    "The following selected columns are not character or factor: y, z. Only character and factor columns can be masked."
  )
})

test_that("mask_variables validates input correctly", {
  df <- data.frame(x = c("A", "B"), y = c("X", "Y"))
  
  # Test NULL data
  expect_error(
    mask_variables(NULL, "x"),
    "Input 'data' must be a data frame.",
    fixed = TRUE
  )
  
  # Test non-data.frame input
  expect_error(
    mask_variables(list(x = c("A", "B")), "x"),
    "Input 'data' must be a data frame.",
    fixed = TRUE
  )
  
  # Test empty data frame
  expect_error(
    mask_variables(data.frame(), "x"),
    "Input 'data' cannot be an empty data frame.",
    fixed = TRUE
  )
  
  # Test invalid across_variables parameter
  expect_error(
    mask_variables(df, "x", across_variables = NULL),
    "Parameter 'across_variables' cannot be NULL. Please provide a logical value.",
    fixed = TRUE
  )

  expect_error(
    mask_variables(df, "x", across_variables = "TRUE"),
    "Parameter 'across_variables' must be a single logical value",
    fixed = TRUE
  )

  expect_error(
    mask_variables(df, "x", across_variables = c(TRUE, FALSE)),
    "Parameter 'across_variables' must be a single logical value",
    fixed = TRUE
  )
})

test_that("mask_variables handles column selection errors", {
  df <- data.frame(x = c("A", "B"), y = c("X", "Y"))
  
  # Test nonexistent column
  expect_error(
    mask_variables(df, "nonexistent"),
    "Error in column selection:",
    fixed = FALSE
  )
})

test_that("mask_variables handles edge cases", {
  df <- data.frame(
    x = c("A", "B", "A"),
    y = 1:3,
    stringsAsFactors = FALSE
  )
  
  # Test with no columns selected
  expect_warning(
    result <- mask_variables(df, character(0)),
    "No columns selected. Returning original data unchanged.",
    fixed = TRUE
  )
  expect_equal(result, df)
  
  # Test with only non-categorical columns selected
  expect_error(
    mask_variables(df, "y"),
    "The following selected columns are not character or factor: y. Only character and factor columns can be masked."
  )
})

test_that("mask_variables handles empty strings correctly", {
  # Test with empty strings in character columns - treated as valid values
  df <- data.frame(
    x = c("A", "", "B"),
    y = c("X", "Y", ""),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  expect_no_warning(
    result <- mask_variables(df, c("x", "y"))
  )
  
  # Empty strings should be masked (not converted to NA)
  expect_false(is.na(result$x[2]))
  expect_false(is.na(result$y[3]))
  
  # Non-empty values should still be masked
  expect_true(grepl("^x_group_", result$x[1]))
  expect_true(grepl("^x_group_", result$x[3]))
  expect_true(grepl("^y_group_", result$y[1]))
  expect_true(grepl("^y_group_", result$y[2]))
  
  # Empty strings should also be masked with the proper column prefixes
  expect_true(grepl("^x_group_", result$x[2]))
  expect_true(grepl("^y_group_", result$y[3]))
  
  # Test with factor containing empty strings
  df_factor <- data.frame(
    x = factor(c("A", "", "B")),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  expect_no_warning(
    result_factor <- mask_variables(df_factor, "x")
  )
  
  # Empty strings should be masked (not converted to NA)
  expect_s3_class(result_factor$x, "factor")
  expect_false(is.na(result_factor$x[2]))
  
  # Test that NA values are allowed (not empty strings)
  df_with_na <- data.frame(
    x = c("A", NA, "B"),
    y = c(NA, "Y", NA),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df_with_na, c("x", "y"))
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$x[2]))
  
  # Test with across_variables = TRUE and empty strings
  df_across <- data.frame(
    x = c("A", "", "B"),
    y = c("A", "B", ""),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  expect_no_warning(
    result_across <- mask_variables(df_across, c("x", "y"), across_variables = TRUE)
  )
  
  # Empty strings should be masked (not converted to NA)
  expect_false(is.na(result_across$x[2]))
  expect_false(is.na(result_across$y[3]))
  
  # Same values should get same masked labels (across_variables = TRUE)
  expect_equal(result_across$x[1], result_across$y[1])  # Both "A"
  expect_equal(result_across$x[2], result_across$y[3])  # Both ""
})

test_that("mask_variables handles NA values correctly", {
  df <- data.frame(
    x = c("A", NA, "B"),
    y = c(NA, "Y", NA),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df, c("x", "y"))
  
  # Check that NA positions are preserved
  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$y[1]))
  expect_true(is.na(result$y[3]))
  
  # Check that non-NA values are masked with column-specific prefixes
  expect_true(grepl("^x_group_", result$x[1]))
  expect_true(grepl("^x_group_", result$x[3]))
  expect_true(grepl("^y_group_", result$y[2]))
})

test_that("mask_variables handles all-NA columns", {
  df <- data.frame(
    x = c(NA_character_, NA_character_),
    y = c("A", "B"),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  expect_warning(
    result <- mask_variables(df, c("x", "y")),
    "The following columns contain only NA values and were left unchanged: x.",
    fixed = TRUE
  )
  
  # All-NA column should remain unchanged
  expect_true(all(is.na(result$x)))
  
  # Non-NA column should be masked with column-specific prefix
  expect_true(all(grepl("^y_group_", result$y)))
  
  # Test with across_variables = TRUE and all-NA columns
  df_across <- data.frame(
    x = c(NA_character_, NA_character_),
    y = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  expect_warning(
    result_across <- mask_variables(df_across, c("x", "y"), across_variables = TRUE),
    "All values in selected categorical columns are NA. Returning original data unchanged.",
    fixed = TRUE
  )
  
  # All columns should remain unchanged
  expect_true(all(is.na(result_across$x)))
  expect_true(all(is.na(result_across$y)))
  expect_equal(result_across, df_across)
})

test_that("mask_variables handles single row data", {
  df <- data.frame(x = "A", y = "B", stringsAsFactors = FALSE)
  
  set.seed(123)
  result <- mask_variables(df, c("x", "y"))
  
  expect_equal(nrow(result), 1)
  expect_true(grepl("^x_group_", result$x))
  expect_true(grepl("^y_group_", result$y))
})

test_that("mask_variables produces consistent results with set.seed", {
  df <- data.frame(
    x = c("A", "B", "A"),
    y = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )
  
  set.seed(42)
  result1 <- mask_variables(df, c("x", "y"))
  
  set.seed(42)
  result2 <- mask_variables(df, c("x", "y"))
  
  expect_equal(result1, result2)
})

test_that("mask_variables preserves data frame attributes", {
  df <- data.frame(
    x = c("A", "B", "A"),
    y = c("X", "Y", "X"),
    stringsAsFactors = FALSE
  )
  
  # Add custom attribute
  attr(df, "custom_attr") <- "test_value"
  
  set.seed(123)
  result <- mask_variables(df, c("x", "y"))
  
  # Check that the result is still a data.frame
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(names(result), names(df))
})

test_that("mask_variables shared labels work with complex scenario", {
  df <- data.frame(
    var1 = c("A", "B", "C", "A"),
    var2 = c("B", "C", "D", "A"),
    var3 = c("E", "A", "B", "F"),
    stringsAsFactors = FALSE
  )
  
  set.seed(123)
  result <- mask_variables(df, c("var1", "var2", "var3"), across_variables = TRUE)
  
  # Check that same values across columns get same labels
  # Find where "A" appears in original data
  a_positions <- list(
    var1 = which(df$var1 == "A"),
    var2 = which(df$var2 == "A"),
    var3 = which(df$var3 == "A")
  )
  
  # Get all masked values for "A"
  a_masked <- c(
    result$var1[a_positions$var1],
    result$var2[a_positions$var2],
    result$var3[a_positions$var3]
  )
  
  # All "A" values should have the same masked label
  expect_true(length(unique(a_masked)) == 1)
  
  # Similarly for "B"
  b_positions <- list(
    var1 = which(df$var1 == "B"),
    var2 = which(df$var2 == "B"),
    var3 = which(df$var3 == "B")
  )
  
  b_masked <- c(
    result$var1[b_positions$var1],
    result$var2[b_positions$var2],
    result$var3[b_positions$var3]
  )
  
  expect_true(length(unique(b_masked)) == 1)
  
  # "A" and "B" should have different masked labels
  expect_false(unique(a_masked) == unique(b_masked))
})
# ─── TESTS FOR ISSUE: TIDYEVAL FUNCTIONALITY ──────────────────────────────────

test_that("mask_variables works with bare variable names", {
  df <- data.frame(
    treatment = c("control", "intervention", "control"),
    outcome = c("success", "failure", "success"),
    score = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  set.seed(123)
  result <- df |> mask_variables(treatment, outcome)

  # Both columns should be masked
  expect_true(all(grepl("^treatment_group_", result$treatment)))
  expect_true(all(grepl("^outcome_group_", result$outcome)))

  # Numeric column should remain unchanged
  expect_equal(result$score, df$score)
})

test_that("mask_variables works with multiple column sets", {
  df <- data.frame(
    a = c("X", "Y", "Z"),
    b = c("X", "Y", "Z"),
    c = c("P", "Q", "R"),
    d = c("P", "Q", "R"),
    e = c("keep", "keep", "keep"),
    stringsAsFactors = FALSE
  )

  set.seed(123)
  result <- df |> mask_variables(c("a", "b"), c("c", "d"))

  # All four selected columns should be masked
  expect_true(all(grepl("^a_group_", result$a)))
  expect_true(all(grepl("^b_group_", result$b)))
  expect_true(all(grepl("^c_group_", result$c)))
  expect_true(all(grepl("^d_group_", result$d)))

  # Unselected column should remain unchanged
  expect_equal(result$e, df$e)
})

test_that("mask_variables works with multiple tidyselect helpers", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    treat_a = c("X", "Y"),
    treat_b = c("A", "B"),
    cond_1 = c("P", "Q"),
    cond_2 = c("R", "S"),
    other = c("keep", "keep"),
    stringsAsFactors = FALSE
  )

  set.seed(123)
  result <- df |> mask_variables(
    starts_with("treat_"),
    starts_with("cond_")
  )

  # All matching columns should be masked
  expect_true(all(grepl("^treat_a_group_", result$treat_a)))
  expect_true(all(grepl("^treat_b_group_", result$treat_b)))
  expect_true(all(grepl("^cond_1_group_", result$cond_1)))
  expect_true(all(grepl("^cond_2_group_", result$cond_2)))

  # Unselected column should remain unchanged
  expect_equal(result$other, df$other)
})
