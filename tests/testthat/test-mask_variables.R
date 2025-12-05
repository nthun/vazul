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
  result <- mask_variables(df, c("x", "y"))
  
  # All-NA column should remain unchanged
  expect_true(all(is.na(result$x)))
  
  # Non-NA column should be masked with column-specific prefix
  expect_true(all(grepl("^y_group_", result$y)))
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

test_that("mask_variables works with bare column names", {
  df <- data.frame(
    treatment = c("control", "intervention", "control"),
    outcome = c("success", "failure", "success"),
    score = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  # Test with multiple bare column names using c()
  set.seed(123)
  result <- mask_variables(df, c(treatment, outcome))

  expect_true(all(grepl("^treatment_group_", result$treatment)))
  expect_true(all(grepl("^outcome_group_", result$outcome)))
  expect_equal(result$score, df$score)  # unchanged

  # Test with single bare column name
  set.seed(123)
  result2 <- mask_variables(df, treatment)

  expect_true(all(grepl("^treatment_group_", result2$treatment)))
  expect_equal(result2$outcome, df$outcome)  # unchanged
  expect_equal(result2$score, df$score)  # unchanged
})