# Tests for scramble_values_rowwise function

test_that("scramble_values_rowwise works with basic numeric columns", {
    df <- data.frame(
        x = c(1, 4, 7),
        y = c(2, 5, 8),
        z = c(3, 6, 9),
        other = c("a", "b", "c")
    )

    set.seed(123)
    result <- scramble_values_rowwise(df, c("x", "y", "z"))

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(df))
    expect_equal(ncol(result), ncol(df))
    expect_equal(names(result), names(df))

    # Check that scrambling occurred rowwise
    expect_setequal(c(result$x[1], result$y[1], result$z[1]), c(1, 2, 3))
    expect_setequal(c(result$x[2], result$y[2], result$z[2]), c(4, 5, 6))
    expect_setequal(c(result$x[3], result$y[3], result$z[3]), c(7, 8, 9))

    # Check that non-scrambled column is unchanged
    expect_equal(result$other, df$other)
})

test_that("scramble_values_rowwise works with tidyselect expressions", {
    df <- data.frame(
        day_1 = c(1, 4, 7),
        day_2 = c(2, 5, 8),
        day_3 = c(3, 6, 9),
        score_a = c(10, 40, 70),
        id = 1:3
    )

    set.seed(123)
    result <- scramble_values_rowwise(df, starts_with("day_"))

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), names(df))

    # Check that day columns are scrambled within each row
    expect_setequal(c(result$day_1[1], result$day_2[1], result$day_3[1]), c(1, 2, 3))
    expect_setequal(c(result$day_1[2], result$day_2[2], result$day_3[2]), c(4, 5, 6))
    expect_setequal(c(result$day_1[3], result$day_2[3], result$day_3[3]), c(7, 8, 9))

    # Check that non-selected columns are unchanged
    expect_equal(result$score_a, df$score_a)
    expect_equal(result$id, df$id)
})

test_that("scramble_values_rowwise handles single column with warning", {
    df <- data.frame(
        x = 1:5,
        y = letters[1:5]
    )

    expect_warning(
        result <- scramble_values_rowwise(df, "x"),
        "Only one column selected - no scrambling performed.",
        fixed = TRUE
    )

    # Data should be unchanged
    expect_equal(result, df)

    # Test with character vector of one column
    expect_warning(
        result2 <- scramble_values_rowwise(df, c("x")),
        "Only one column selected - no scrambling performed.",
        fixed = TRUE
    )

    expect_equal(result2, df)
})

test_that("scramble_values_rowwise validates input correctly", {
    df <- data.frame(x = 1:5, y = letters[1:5])

    # Test non-data.frame input
    expect_error(
        scramble_values_rowwise(list(x = 1:5), "x"),
        "is.data.frame\\(data\\) is not TRUE",
        fixed = FALSE
    )

    # Test no columns selected (nonexistent column)
    expect_error(
        scramble_values_rowwise(df, "nonexistent_column"),
        "Can't subset columns that don't exist",
        fixed = FALSE
    )

    # Test with empty selection
    expect_error(
        scramble_values_rowwise(df, NULL),
        "No columns selected",
        fixed = FALSE
    )
})

test_that("scramble_values_rowwise handles edge cases", {
    # Test with single row
    df_single <- data.frame(x = 1, y = 2, z = 3)
    result <- scramble_values_rowwise(df_single, c("x", "y", "z"))
    expect_equal(nrow(result), 1)
    expect_setequal(c(result$x, result$y, result$z), c(1, 2, 3))

    # Test with two columns only
    df_two <- data.frame(a = c(1, 3), b = c(2, 4))
    set.seed(123)
    result <- scramble_values_rowwise(df_two, c("a", "b"))
    expect_setequal(c(result$a[1], result$b[1]), c(1, 2))
    expect_setequal(c(result$a[2], result$b[2]), c(3, 4))
})

test_that("scramble_values_rowwise actually scrambles data (probabilistic)", {
    # Create larger dataset to ensure scrambling occurs
    set.seed(42)
    df <- data.frame(
        x = 1:20,
        y = 21:40,
        z = 41:60,
        other = letters[1:20]
    )

    result <- scramble_values_rowwise(df, c("x", "y", "z"))

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

test_that("scramble_values_rowwise preserves data types", {
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

    # Test with integer columns
    set.seed(123)
    result_int <- scramble_values_rowwise(df, c("int_col1", "int_col2"))

    expect_type(result_int$int_col1, "integer")
    expect_type(result_int$int_col2, "integer")
    expect_setequal(c(result_int$int_col1[1], result_int$int_col2[1]), c(1L, 4L))

    # Test with numeric columns
    set.seed(123)
    result_num <- scramble_values_rowwise(df, c("num_col1", "num_col2"))

    expect_type(result_num$num_col1, "double")
    expect_type(result_num$num_col2, "double")
    expect_setequal(c(result_num$num_col1[1], result_num$num_col2[1]), c(1.1, 4.4))

    # Test with character columns
    set.seed(123)
    result_char <- scramble_values_rowwise(df, c("char_col1", "char_col2"))

    expect_type(result_char$char_col1, "character")
    expect_type(result_char$char_col2, "character")
    expect_setequal(c(result_char$char_col1[1], result_char$char_col2[1]), c("a", "x"))

    # Check that other columns are unchanged
    expect_equal(result_int$factor_col, df$factor_col)
    expect_equal(result_int$date_col, df$date_col)
    expect_equal(result_int$logical_col, df$logical_col)
    expect_equal(result_int$other, df$other)
})

test_that("scramble_values_rowwise preserves NA values correctly", {
    df <- data.frame(
        x = c(1, NA, 3, 4),
        y = c(10, 20, NA, 40),
        z = c(100, 200, 300, NA),
        other = letters[1:4]
    )

    set.seed(123)
    result <- scramble_values_rowwise(df, c("x", "y", "z"))

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

test_that("scramble_values_rowwise works with column indices", {
    df <- data.frame(
        a = c(1, 4),
        b = c(2, 5),
        c = c(3, 6)
    )

    set.seed(123)
    result <- scramble_values_rowwise(df, c(1, 2, 3))

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), names(df))
    expect_setequal(c(result$a[1], result$b[1], result$c[1]), c(1, 2, 3))
    expect_setequal(c(result$a[2], result$b[2], result$c[2]), c(4, 5, 6))
})

test_that("scramble_values_rowwise works with advanced tidyselect helpers", {
    df <- data.frame(
        day_1 = c(1, 4, 7),
        day_2 = c(2, 5, 8),
        score_a = c(10, 40, 70),
        score_b = c(20, 50, 80),
        id = 1:3,
        category = c("A", "B", "C")
    )

    # Test with contains
    set.seed(123)
    result1 <- scramble_values_rowwise(df, contains("day"))
    expect_setequal(c(result1$day_1[1], result1$day_2[1]), c(1, 2))
    expect_equal(result1$score_a, df$score_a)
    expect_equal(result1$category, df$category)

    # Test with where (numeric columns) — integer + double is allowed without warning
    set.seed(123)
    result2 <- scramble_values_rowwise(df, where(is.numeric))

    # Should return a data frame with same dimensions
    expect_s3_class(result2, "data.frame")
    expect_equal(nrow(result2), 3)
    expect_equal(ncol(result2), ncol(df))

    # Check row 1 has the same values as original row 1 for numeric columns
    numeric_cols <- c("day_1", "day_2", "score_a", "score_b", "id")
    original_numeric_row1 <- unname(unlist(df[1, numeric_cols, drop = TRUE]))
    result_numeric_row1 <- unname(unlist(result2[1, numeric_cols, drop = TRUE]))
    expect_setequal(original_numeric_row1, result_numeric_row1)

    # Character column should be unchanged
    expect_equal(result2$category, df$category)
})

test_that("scramble_values_rowwise preserves input data frame type", {
    skip_if_not_installed("dplyr")

    # Test data
    df <- data.frame(x = 1:5, y = letters[1:5], z = 6:10)
    tbl <- dplyr::tibble(x = 1:5, y = letters[1:5], z = 6:10)

    # Test with data.frame input
    set.seed(123)
    result_df <- scramble_values_rowwise(df, c("x", "z"))
    expect_equal(class(result_df), class(df))

    # Test with tibble input
    set.seed(123)
    result_tbl <- scramble_values_rowwise(tbl, c("x", "z"))
    expect_equal(class(result_tbl), class(tbl))
})

test_that("scramble_values_rowwise warns on mixed column types", {
    df <- data.frame(
        num = c(1, 2, 3),
        char = c("a", "b", "c"),
        stringsAsFactors = FALSE
    )

    expect_warning(
        result <- scramble_values_rowwise(df, c("num", "char")),
        "Columns have mixed types: double, character. Scrambling may cause coercion.",
        fixed = TRUE
    )

    # Still should scramble — but with coercion to character
    expect_type(result$num, "character")
    expect_type(result$char, "character")
    expect_setequal(c(result$num[1], result$char[1]), c("1", "a"))
    expect_setequal(c(result$num[2], result$char[2]), c("2", "b"))
    expect_setequal(c(result$num[3], result$char[3]), c("3", "c"))
})

test_that("scramble_values_rowwise does not warn for integer + double", {
    df <- data.frame(
        int_col = c(1L, 2L),
        dbl_col = c(3.3, 4.4)
    )

    w <- NULL
    result <- withCallingHandlers(
        scramble_values_rowwise(df, c("int_col", "dbl_col")),
        warning = function(warn) {
            w <<- c(w, conditionMessage(warn))
            invokeRestart("muffleWarning")
        }
    )

    expect_null(w)  # No warnings
    expect_type(result$int_col, "double")  # Coerced to double
    expect_type(result$dbl_col, "double")
    expect_setequal(c(result$int_col[1], result$dbl_col[1]), c(1, 3.3))
})
