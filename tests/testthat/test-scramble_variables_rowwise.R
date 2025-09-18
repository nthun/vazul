# Tests for scramble_variables_rowwise function

library(vazul)

test_that("scramble_variables_rowwise works with single variable set", {
  # Create test data frame
  df <- data.frame(
    x = c(1, 4, 7),
    y = c(2, 5, 8),
    z = c(3, 6, 9),
    other = c("a", "b", "c")
  )
  
  # Test scrambling by column names
  set.seed(123)
  result <- scramble_variables_rowwise(df, c("x", "y", "z"))
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  expect_equal(names(result), names(df))
  
  # Check that scrambling occurred rowwise
  # Row 1: original (1,2,3) should be scrambled but contain same values
  expect_setequal(c(result$x[1], result$y[1], result$z[1]), c(1, 2, 3))
  expect_setequal(c(result$x[2], result$y[2], result$z[2]), c(4, 5, 6))
  expect_setequal(c(result$x[3], result$y[3], result$z[3]), c(7, 8, 9))
  
  # Check that non-scrambled column is unchanged
  expect_equal(result$other, df$other)
})

test_that("scramble_variables_rowwise works with multiple variable sets", {
  df <- data.frame(
    day_1 = c(1, 4, 7),
    day_2 = c(2, 5, 8),
    day_3 = c(3, 6, 9),
    score_a = c(10, 40, 70),
    score_b = c(20, 50, 80),
    id = 1:3
  )
  
  set.seed(123)
  result <- scramble_variables_rowwise(df, list(
    c("day_1", "day_2", "day_3"),
    c("score_a", "score_b")
  ))
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  expect_equal(names(result), names(df))
  
  # Check that each row's day values are scrambled within the row
  expect_setequal(c(result$day_1[1], result$day_2[1], result$day_3[1]), c(1, 2, 3))
  expect_setequal(c(result$day_1[2], result$day_2[2], result$day_3[2]), c(4, 5, 6))
  
  # Check that each row's score values are scrambled within the row
  expect_setequal(c(result$score_a[1], result$score_b[1]), c(10, 20))
  expect_setequal(c(result$score_a[2], result$score_b[2]), c(40, 50))
  
  # Check that non-scrambled column is unchanged
  expect_equal(result$id, df$id)
})

test_that("scramble_variables_rowwise works with column indices", {
  df <- data.frame(
    a = c(1, 4),
    b = c(2, 5),
    c = c(3, 6)
  )
  
  set.seed(123)
  result <- scramble_variables_rowwise(df, c(1, 2, 3))
  
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), names(df))
  expect_setequal(c(result$a[1], result$b[1], result$c[1]), c(1, 2, 3))
  expect_setequal(c(result$a[2], result$b[2], result$c[2]), c(4, 5, 6))
})

test_that("scramble_variables_rowwise handles single column sets", {
  df <- data.frame(
    x = 1:5,
    y = letters[1:5],
    z = 6:10
  )
  
  # Single column in a set should not be scrambled (nothing to scramble)
  result <- scramble_variables_rowwise(df, "x")
  expect_equal(result$x, df$x)
  expect_equal(result$y, df$y)
  expect_equal(result$z, df$z)
  
  # Single column in a list
  result2 <- scramble_variables_rowwise(df, list("x"))
  expect_equal(result2$x, df$x)
})

test_that("scramble_variables_rowwise validates input correctly", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  
  # Test non-data.frame input
  expect_error(
    scramble_variables_rowwise(list(x = 1:5), "x"),
    class = "simpleError"
  )
  
  # Test empty variable_sets
  expect_error(
    scramble_variables_rowwise(df, list()),
    "variable_sets cannot be empty"
  )
  
  # Test missing columns
  expect_error(
    scramble_variables_rowwise(df, "nonexistent_column"),
    "Can't subset columns that don't exist"
  )
  
  # Test invalid list elements
  expect_error(
    scramble_variables_rowwise(df, list(data.frame(a = 1))),
    "must be a character vector of column names or numeric vector"
  )
})

test_that("scramble_variables_rowwise handles edge cases", {
  # Test with single row
  df_single <- data.frame(x = 1, y = 2, z = 3)
  result <- scramble_variables_rowwise(df_single, c("x", "y", "z"))
  expect_equal(nrow(result), 1)
  expect_setequal(c(result$x, result$y, result$z), c(1, 2, 3))
  
  # Test with two columns only
  df_two <- data.frame(a = c(1, 3), b = c(2, 4))
  set.seed(123)
  result <- scramble_variables_rowwise(df_two, c("a", "b"))
  expect_setequal(c(result$a[1], result$b[1]), c(1, 2))
  expect_setequal(c(result$a[2], result$b[2]), c(3, 4))
})

test_that("scramble_variables_rowwise actually scrambles data (probabilistic)", {
  # Create larger dataset to ensure scrambling occurs
  set.seed(42)  # Different seed to ensure we get scrambling
  df <- data.frame(
    x = 1:20,
    y = 21:40,
    z = 41:60,
    other = letters[1:20]
  )
  
  result <- scramble_variables_rowwise(df, c("x", "y", "z"))
  
  # At least some rows should have been scrambled
  scrambled_rows <- 0
  for (i in 1:nrow(df)) {
    original <- c(df$x[i], df$y[i], df$z[i])
    scrambled <- c(result$x[i], result$y[i], result$z[i])
    if (!identical(original, scrambled)) {
      scrambled_rows <- scrambled_rows + 1
    }
  }
  
  # With 20 rows of 3 elements each, we expect some scrambling
  expect_gt(scrambled_rows, 0)
  
  # But other column should be unchanged
  expect_equal(result$other, df$other)
})

test_that("scramble_variables_rowwise preserves data types", {
  df <- data.frame(
    int_col1 = c(1L, 2L, 3L),
    int_col2 = c(4L, 5L, 6L),
    num_col1 = c(1.1, 2.2, 3.3),
    num_col2 = c(4.4, 5.5, 6.6),
    char_col1 = c("a", "b", "c"),
    char_col2 = c("x", "y", "z"),
    factor_col = factor(c("low", "med", "high")),
    date_col = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    logical_col = c(TRUE, FALSE, TRUE),
    other = 1:3
  )
  
  set.seed(123)
  result <- scramble_variables_rowwise(df, list(
    c("int_col1", "int_col2"),  # integers together
    c("num_col1", "num_col2"),  # numerics together  
    c("char_col1", "char_col2") # characters together
  ))
  
  expect_s3_class(result, "data.frame")
  
  # Check types are preserved
  expect_type(result$int_col1, "integer")
  expect_type(result$int_col2, "integer")
  expect_type(result$num_col1, "double")
  expect_type(result$num_col2, "double")
  expect_type(result$char_col1, "character")
  expect_type(result$char_col2, "character")
  expect_s3_class(result$factor_col, "factor")
  expect_s3_class(result$date_col, "Date")
  expect_type(result$logical_col, "logical")
  
  # Check unchanged columns
  expect_equal(result$factor_col, df$factor_col)
  expect_equal(result$date_col, df$date_col)
  expect_equal(result$logical_col, df$logical_col)
  expect_equal(result$other, df$other)
  
  # Check that scrambling occurred within same-type sets
  # Row 1: integers should be scrambled
  expect_setequal(c(result$int_col1[1], result$int_col2[1]), c(1L, 4L))
  # Row 1: numerics should be scrambled  
  expect_setequal(c(result$num_col1[1], result$num_col2[1]), c(1.1, 4.4))
  # Row 1: characters should be scrambled
  expect_setequal(c(result$char_col1[1], result$char_col2[1]), c("a", "x"))
})

test_that("scramble_variables_rowwise preserves NA values correctly", {
  df <- data.frame(
    x = c(1, NA, 3, 4),
    y = c(10, 20, NA, 40),
    z = c(100, 200, 300, NA),
    other = letters[1:4]
  )
  
  set.seed(123)
  result <- scramble_variables_rowwise(df, c("x", "y", "z"))
  
  # Check that NA counts are preserved in each row
  for (i in 1:nrow(df)) {
    original_na_count <- sum(is.na(c(df$x[i], df$y[i], df$z[i])))
    result_na_count <- sum(is.na(c(result$x[i], result$y[i], result$z[i])))
    expect_equal(result_na_count, original_na_count)
    
    # Check that non-NA values are preserved within each row
    original_values <- c(df$x[i], df$y[i], df$z[i])
    result_values <- c(result$x[i], result$y[i], result$z[i])
    expect_setequal(original_values[!is.na(original_values)], 
                    result_values[!is.na(result_values)])
  }
  
  expect_equal(result$other, df$other)
})