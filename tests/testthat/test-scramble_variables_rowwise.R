# Tests for scramble_variables_rowwise function

test_that("scramble_variables_rowwise works with single variable set", {
    df <- data.frame(
        x = c(1, 4, 7),
        y = c(2, 5, 8),
        z = c(3, 6, 9),
        other = c("a", "b", "c")
    )

    set.seed(123)
    result <- expect_warning(
        scramble_variables_rowwise(df, c("x", "y", "z")),
        NA  # expect NO warning
    )

    expect_s3_class(result, "data.frame")
    expect_equal(dim(result), dim(df))
    expect_equal(names(result), names(df))

    # Check scrambling rowwise — robust to 1 or N rows
    orig_list <- asplit(df[c("x", "y", "z")], 1)
    scrambled_list <- asplit(result[c("x", "y", "z")], 1)
    expect_true(all(mapply(setequal, orig_list, scrambled_list)))

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
    result <- expect_warning(
        scramble_variables_rowwise(df,
                                   c("day_1", "day_2", "day_3"),
                                   c("score_a", "score_b")
        ),
        NA
    )

    expect_s3_class(result, "data.frame")
    expect_equal(dim(result), dim(df))
    expect_equal(names(result), names(df))

    # All column sets are combined into one set, so values can move between day_* and score_* columns.
    orig_list <- asplit(df[c("day_1", "day_2", "day_3", "score_a", "score_b")], 1)
    scrambled_list <- asplit(result[c("day_1", "day_2", "day_3", "score_a", "score_b")], 1)
    expect_true(all(mapply(setequal, orig_list, scrambled_list)))

    expect_equal(result$id, df$id)
})

test_that("scramble_variables_rowwise works with column indices", {
    df <- data.frame(
        a = c(1, 4),
        b = c(2, 5),
        c = c(3, 6)
    )

    set.seed(123)
    result <- expect_warning(
        scramble_variables_rowwise(df, c(1, 2, 3)),
        NA
    )

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), names(df))

    orig_list <- asplit(df, 1)
    scr_list <- asplit(result, 1)
    expect_true(all(mapply(setequal, orig_list, scr_list)))
})

test_that("scramble_variables_rowwise handles single column sets", {
    df <- data.frame(
        x = 1:5,
        y = letters[1:5],
        z = 6:10
    )

    # Single column as character string should work and warn about only one column
    expect_warning(
        result <- scramble_variables_rowwise(df, "x"),
        "Only one column selected. Rowwise scrambling requires at least 2 columns.",
        fixed = TRUE
    )
    expect_equal(result, df)

    expect_warning(
        result2 <- scramble_variables_rowwise(df, c("x")),
        "Only one column selected. Rowwise scrambling requires at least 2 columns.",
        fixed = TRUE
    )
    expect_equal(result2, df)
})

test_that("scramble_variables_rowwise validates input correctly", {
    df <- data.frame(x = 1:5, y = letters[1:5])

    expect_error(
        scramble_variables_rowwise(list(x = 1:5), "x"),
        "Input 'data' must be a data frame.",
        fixed = TRUE
    )

    expect_warning(
        result <- scramble_variables_rowwise(df),
        "No columns selected. Returning original data unchanged.",
        fixed = TRUE
    )
    expect_equal(result, df)

    expect_error(
        scramble_variables_rowwise(df, "nonexistent_column"),
        "Error in column selection: Can't subset columns that don't exist.",
        fixed = FALSE
    )

    expect_error(
        scramble_variables_rowwise(df, data.frame(a = 1)),
        "Error in column selection:",
        fixed = TRUE
    )
})

test_that("scramble_variables_rowwise handles edge cases", {
    df_single <- data.frame(x = 1, y = 2, z = 3)
    result <- expect_warning(
        scramble_variables_rowwise(df_single, c("x", "y", "z")),
        NA
    )
    expect_equal(nrow(result), 1)
    expect_setequal(unlist(result[1, c("x", "y", "z")]), c(1, 2, 3))

    df_two <- data.frame(a = c(1, 3), b = c(2, 4))
    set.seed(123)
    result <- expect_warning(
        scramble_variables_rowwise(df_two, c("a", "b")),
        NA
    )
    orig_pairs <- asplit(df_two, 1)
    scr_pairs <- asplit(result[c("a", "b")], 1)
    expect_true(all(mapply(setequal, orig_pairs, scr_pairs)))
})

test_that("scramble_variables_rowwise actually scrambles data (probabilistic)", {
    set.seed(42)
    df <- data.frame(
        x = 1:20,
        y = 21:40,
        z = 41:60,
        other = letters[1:20]
    )

    result <- expect_warning(
        scramble_variables_rowwise(df, c("x", "y", "z")),
        NA
    )

    # Vectorized check for scrambling
    orig_rows <- asplit(df[c("x", "y", "z")], 1)
    scr_rows <- asplit(result[c("x", "y", "z")], 1)

    scrambled <- mapply(function(orig, scr) !identical(orig, scr), orig_rows, scr_rows)

    expect_gt(sum(scrambled), 0)
    expect_equal(result$other, df$other)
})

test_that("scramble_variables_rowwise preserves NA values correctly", {
    df <- data.frame(
        x = c(1, NA, 3, 4),
        y = c(10, 20, NA, 40),
        z = c(100, 200, 300, NA),
        other = letters[1:4]
    )

    set.seed(123)
    result <- expect_warning(
        scramble_variables_rowwise(df, c("x", "y", "z")),
        NA
    )

    orig_rows <- asplit(df[c("x", "y", "z")], 1)
    scr_rows <- asplit(result[c("x", "y", "z")], 1)

    checks <- mapply(function(orig, scr) {
        # Check NA count
        if (sum(is.na(orig)) != sum(is.na(scr))) return(FALSE)
        # Check values (excluding NA)
        setequal(orig[!is.na(orig)], scr[!is.na(scr)])
    }, orig_rows, scr_rows)

    expect_true(all(checks))
    expect_equal(result$other, df$other)
})

test_that("scramble_variables_rowwise preserves data types", {
    df <- data.frame(
        int_col1 = c(1L, 2L, 3L),
        int_col2 = c(4L, 5L, 6L),
        date_col1 = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
        date_col2 = as.Date(c("2023-01-04", "2023-01-05", "2023-01-06")),
        factor_col1 = factor(c("low", "med", "high"), levels = c("low", "med", "high")),
        factor_col2 = factor(c("med", "high", "low"), levels = c("low", "med", "high")),
        logical_col = c(TRUE, FALSE, TRUE),
        other = 1:3
    )

    set.seed(123)
    result <- expect_warning(
        scramble_variables_rowwise(df,
                                   c("int_col1", "int_col2")
        ),
        NA
    )

    expect_s3_class(result, "data.frame")

    expect_type(result$int_col1, "integer")
    expect_type(result$int_col2, "integer")
    expect_s3_class(result$date_col1, "Date")
    expect_s3_class(result$date_col2, "Date")
    expect_s3_class(result$factor_col1, "factor")
    expect_s3_class(result$factor_col2, "factor")
    expect_type(result$logical_col, "logical")

    # Unselected columns remain unchanged
    expect_equal(result[c("date_col1", "date_col2", "factor_col1", "factor_col2", "logical_col", "other")],
                 df[c("date_col1", "date_col2", "factor_col1", "factor_col2", "logical_col", "other")])

    # Spot-check scrambling within sets
    expect_setequal(c(result$int_col1[1], result$int_col2[1]), c(1L, 4L))
})

test_that("scramble_variables_rowwise works with tidyselect expressions", {
    df <- data.frame(
        day_1 = c(1, 4, 7),
        day_2 = c(2, 5, 8),
        day_3 = c(3, 6, 9),
        score_a = c(10, 40, 70),
        score_b = c(20, 50, 80),
        id = 1:3
    )

    set.seed(123)
    result1 <- expect_warning(
        scramble_variables_rowwise(df, starts_with("day_")),
        NA
    )
    expect_equal(names(result1), names(df))
    day_orig_list <- asplit(df[c("day_1", "day_2", "day_3")], 1)
    day_scr_list <- asplit(result1[c("day_1", "day_2", "day_3")], 1)
    expect_true(all(mapply(setequal, day_orig_list, day_scr_list)))

    set.seed(123)
    result2 <- expect_warning(
        scramble_variables_rowwise(df,
                                   starts_with("day_"),
                                   starts_with("score_")
        ),
        NA
    )
    expect_equal(names(result2), names(df))
    # Selections are combined into one set, so day and score values can swap positions.
    orig_list <- asplit(df[c("day_1", "day_2", "day_3", "score_a", "score_b")], 1)
    scrambled_list <- asplit(result2[c("day_1", "day_2", "day_3", "score_a", "score_b")], 1)
    expect_true(all(mapply(setequal, orig_list, scrambled_list)))
    expect_equal(result2$id, df$id)
})

test_that("scramble_variables_rowwise preserves input data frame type", {
    skip_if_not_installed("dplyr")

    df <- data.frame(x = 1:5, y = letters[1:5], z = 6:10)
    tbl <- dplyr::tibble(x = 1:5, y = letters[1:5], z = 6:10)

    set.seed(123)
    result_df <- expect_warning(
        scramble_variables_rowwise(df, c("x", "z")),  # ← both numeric
        NA
    )
    expect_equal(class(result_df), class(df))

    set.seed(123)
    result_tbl <- expect_warning(
        scramble_variables_rowwise(tbl, c("x", "z")),  # ← both numeric
        NA
    )
    expect_equal(class(result_tbl), class(tbl))
})

test_that("scramble_variables_rowwise errors on incompatible types/classes", {
    df <- data.frame(
        num = c(1, 2, 3),
        chr = c("a", "b", "c"),
        stringsAsFactors = FALSE
    )

    expect_error(
        scramble_variables_rowwise(df, c("num", "chr")),
        "Rowwise scrambling requires selected columns to have the same class",
        fixed = TRUE
    )
})


# ─── TESTS FOR ISSUE: TIDYEVAL FUNCTIONALITY ──────────────────────────────────

test_that("scramble_variables_rowwise works with tidyselect all_of for column set", {
  df <- data.frame(
    a = c(1, 4, 7),
    b = c(2, 5, 8),
    c = c(3, 6, 9),
    other = c("x", "y", "z")
  )

  set.seed(123)
  # Use all_of() or c() to specify columns as a single set
  result <- df |> scramble_variables_rowwise(c("a", "b", "c"))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))

  # Values should be scrambled rowwise - each row should have same elements
  for (i in seq_len(nrow(df))) {
    orig_vals <- as.numeric(df[i, c("a", "b", "c")])
    result_vals <- as.numeric(result[i, c("a", "b", "c")])
    expect_setequal(result_vals, orig_vals)
  }

  # Unselected column should remain unchanged
  expect_equal(result$other, df$other)
})

test_that("scramble_variables_rowwise combines multiple column sets into a single set", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(10, 20, 30),
    c = c(100, 200, 300),
    d = c(1000, 2000, 3000)
  )

  set.seed(42)  # Use seed where scrambling is visible
  result <- df |> scramble_variables_rowwise(c("a", "b"), c("c", "d"))

  expect_s3_class(result, "data.frame")

  # Values should be scrambled rowwise within the combined set
  for (i in seq_len(nrow(df))) {
    orig_vals <- as.numeric(df[i, c("a", "b", "c", "d")])
    result_vals <- as.numeric(result[i, c("a", "b", "c", "d")])
    expect_setequal(result_vals, orig_vals)
  }
})

test_that("scramble_variables_rowwise works with multiple tidyselect helpers", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    day_1 = c(1, 4, 7),
    day_2 = c(2, 5, 8),
    day_3 = c(3, 6, 9),
    score_a = c(10, 40, 70),
    score_b = c(20, 50, 80),
    id = 1:3
  )

  set.seed(123)
  result <- df |> scramble_variables_rowwise(
    starts_with("day_"),
    starts_with("score_")
  )

  # Selections are combined into one set, so values can move between day and score columns.
  for (i in seq_len(nrow(df))) {
    orig_vals <- as.numeric(df[i, c("day_1", "day_2", "day_3", "score_a", "score_b")])
    result_vals <- as.numeric(result[i, c("day_1", "day_2", "day_3", "score_a", "score_b")])
    expect_setequal(result_vals, orig_vals)
  }

  # id column should remain unchanged
  expect_equal(result$id, df$id)
})

test_that("scramble_variables_rowwise treats bare column names as a combined set", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(10, 20, 30),
    c = c(100, 200, 300)
  )

  set.seed(123)
  result <- expect_warning(
    scramble_variables_rowwise(df, a, b),
    NA
  )

  for (i in seq_len(nrow(df))) {
    expect_setequal(as.numeric(result[i, c("a", "b")]), as.numeric(df[i, c("a", "b")]))
  }

  expect_equal(result$c, df$c)
})
