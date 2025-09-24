# Tests for mask_variables_rowwise function

test_that("mask_variables_rowwise works with single variable set", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c("B", "C", "A"),
    z = c("C", "A", "B"),
    other = c("D", "E", "F")
  )

  set.seed(123)
  result <- expect_warning(
    mask_variables_rowwise(df, c("x", "y", "z")),
    NA  # expect NO warning
  )

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(df))
  expect_equal(names(result), names(df))

  # Check that masking was applied to selected columns
  expect_true(all(grepl("^masked_group_", result$x)))
  expect_true(all(grepl("^masked_group_", result$y)))
  expect_true(all(grepl("^masked_group_", result$z)))
  
  # Check that non-selected columns remained unchanged
  expect_equal(result$other, df$other)
  
  # Check that same original values get same masked labels across columns
  # For example, all "A"s should get the same masked label
  original_a_positions <- which(unlist(df[c("x", "y", "z")]) == "A")
  masked_a_values <- unlist(result[c("x", "y", "z")])[original_a_positions]
  expect_true(all(masked_a_values == masked_a_values[1]))
})

test_that("mask_variables_rowwise works with multiple variable sets", {
  df <- data.frame(
    treat_1 = c("control", "treatment", "placebo"),
    treat_2 = c("treatment", "placebo", "control"),
    cond_a = c("X", "Y", "X"),
    cond_b = c("Y", "X", "Y"),
    id = 1:3
  )

  set.seed(456)
  result <- expect_warning(
    mask_variables_rowwise(df, c("treat_1", "treat_2"), c("cond_a", "cond_b")),
    NA
  )

    expect_s3_class(result, "data.frame")
    expect_equal(dim(result), dim(df))
    expect_equal(names(result), names(df))

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
    expect_equal(dim(result), dim(df))
    
    # Only treat_ columns should be masked
    expect_true(all(grepl("^masked_group_", result$treat_1)))
    expect_true(all(grepl("^masked_group_", result$treat_2)))
    
    # Other columns should remain unchanged
    expect_equal(result$other_1, df$other_1)
    expect_equal(result$other_2, df$other_2)
    expect_equal(result$id, df$id)
})

test_that("mask_variables_rowwise uses labels from all rows as specified", {
    # Create data where not all values appear in each row
    df <- data.frame(
        x = c("A", "B", "A"),
        y = c("B", "C", "A"),  # "C" only appears in row 2
        z = c("A", "A", "D")   # "D" only appears in row 3
    )
    
    set.seed(111)
    result <- mask_variables_rowwise(df, c("x", "y", "z"))
    
    # All unique values (A, B, C, D) should be in the mapping
    # even though they don't all appear in every row
    all_original <- unique(unlist(df[c("x", "y", "z")]))
    all_masked <- unique(unlist(result[c("x", "y", "z")]))
    
    # Should have 4 unique masked labels for A, B, C, D
    expect_equal(length(all_masked), length(all_original))
    expect_equal(length(all_masked), 4)
})

test_that("mask_variables_rowwise preserves factor levels correctly", {
    df <- data.frame(
        x = factor(c("Low", "High", "Medium"), levels = c("Low", "Medium", "High")),
        y = factor(c("High", "Low", "High"), levels = c("Low", "Medium", "High"))
    )

    set.seed(222)
    result <- mask_variables_rowwise(df, c("x", "y"))

    expect_s3_class(result$x, "factor")
    expect_s3_class(result$y, "factor")
    
    # Should have 3 masked levels
    expect_equal(length(levels(result$x)), 3)
    expect_equal(length(levels(result$y)), 3)
    
    # All levels should start with prefix
    expect_true(all(grepl("^masked_group_", levels(result$x))))
    expect_true(all(grepl("^masked_group_", levels(result$y))))
})

test_that("mask_variables_rowwise handles NA values correctly", {
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

test_that("mask_variables_rowwise uses custom prefix correctly", {
    df <- data.frame(
        x = c("A", "B", "C"),
        y = c("B", "C", "A")
    )

    set.seed(444)
    result <- mask_variables_rowwise(df, c("x", "y"), prefix = "custom_")

    expect_true(all(grepl("^custom_", result$x)))
    expect_true(all(grepl("^custom_", result$y)))
    expect_false(any(grepl("^masked_group_", result$x)))
    expect_false(any(grepl("^masked_group_", result$y)))
})

test_that("mask_variables_rowwise produces consistent results with set.seed", {
    df <- data.frame(
        x = c("A", "B", "C"),
        y = c("B", "C", "A")
    )

    set.seed(555)
    result1 <- mask_variables_rowwise(df, c("x", "y"))

    set.seed(555)
    result2 <- mask_variables_rowwise(df, c("x", "y"))

    expect_equal(result1, result2)
})

test_that("mask_variables_rowwise validates input correctly", {
    df <- data.frame(x = c("A", "B", "C"), y = c("X", "Y", "Z"))

    # Test non-data.frame input
    expect_error(
        mask_variables_rowwise(list(x = c("A", "B", "C")), "x"),
        class = "simpleError"
    )

    # Test with no column sets
    expect_warning(
        result <- mask_variables_rowwise(df),
        "No column sets provided",
        fixed = FALSE
    )
    expect_equal(result, df)

    # Test with nonexistent columns
    expect_warning(
        mask_variables_rowwise(df, "nonexistent_column"),
        "Some column names not found",
        fixed = FALSE
    )

    # Test with invalid column set type
    expect_warning(
        mask_variables_rowwise(df, data.frame(a = 1)),
        "Failed to evaluate column set",
        fixed = FALSE
    )
})

test_that("mask_variables_rowwise handles mixed data types correctly", {
    # Should only work with character/factor columns
    df <- data.frame(
        x = c("A", "B", "C"),
        y = c(1, 2, 3),  # numeric
        z = c("X", "Y", "Z")
    )

    expect_error(
        mask_variables_rowwise(df, c("x", "y", "z")),
        "All selected columns must be character or factor vectors",
        fixed = FALSE
    )
})

test_that("mask_variables_rowwise handles edge cases", {
    # Test single row
    df_single <- data.frame(x = "A", y = "B", z = "C")
    result <- expect_warning(
        mask_variables_rowwise(df_single, c("x", "y", "z")),
        NA
    )
    expect_equal(nrow(result), 1)
    expect_true(all(grepl("^masked_group_", unlist(result[c("x", "y", "z")]))))

    # Test single column (should still work)
    df_single_col <- data.frame(x = c("A", "B", "C"), y = c("X", "Y", "Z"))
    result <- expect_warning(
        mask_variables_rowwise(df_single_col, "x"),
        NA
    )
    expect_true(all(grepl("^masked_group_", result$x)))
    expect_equal(result$y, df_single_col$y)
})

test_that("mask_variables_rowwise maintains consistent mapping within column set", {
    # Test that same values get same masked labels within a column set
    df <- data.frame(
        x = c("A", "B", "A", "C", "B"),
        y = c("B", "A", "C", "A", "C")
    )

    set.seed(666)
    result <- mask_variables_rowwise(df, c("x", "y"))

    # All "A"s should get the same masked label
    a_positions <- which(unlist(df[c("x", "y")]) == "A")
    a_masked_values <- unlist(result[c("x", "y")])[a_positions]
    expect_true(all(a_masked_values == a_masked_values[1]))

    # All "B"s should get the same masked label
    b_positions <- which(unlist(df[c("x", "y")]) == "B")
    b_masked_values <- unlist(result[c("x", "y")])[b_positions]
    expect_true(all(b_masked_values == b_masked_values[1]))

    # All "C"s should get the same masked label
    c_positions <- which(unlist(df[c("x", "y")]) == "C")
    c_masked_values <- unlist(result[c("x", "y")])[c_positions]
    expect_true(all(c_masked_values == c_masked_values[1]))
})