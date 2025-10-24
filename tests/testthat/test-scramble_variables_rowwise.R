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

    # Check day columns
    day_orig_list <- asplit(df[c("day_1", "day_2", "day_3")], 1)
    day_scr_list <- asplit(result[c("day_1", "day_2", "day_3")], 1)
    expect_true(all(mapply(setequal, day_orig_list, day_scr_list)))

    # Check score columns
    score_orig_list <- asplit(df[c("score_a", "score_b")], 1)
    score_scr_list <- asplit(result[c("score_a", "score_b")], 1)
    expect_true(all(mapply(setequal, score_orig_list, score_scr_list)))

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

    expect_warning(
        result <- scramble_variables_rowwise(df, "x"),
        "Only one column selected",
        fixed = FALSE
    )
    expect_equal(result, df)

    expect_warning(
        result2 <- scramble_variables_rowwise(df, c("x")),
        "Only one column selected",
        fixed = FALSE
    )
    expect_equal(result2, df)
})

test_that("scramble_variables_rowwise validates input correctly", {
    df <- data.frame(x = 1:5, y = letters[1:5])

    expect_error(
        scramble_variables_rowwise(list(x = 1:5), "x"),
        class = "simpleError"
    )

    expect_warning(
        result <- scramble_variables_rowwise(df),
        "No column sets provided",
        fixed = FALSE
    )
    expect_equal(result, df)

    # Capture all warnings and test them individually
        w <- capture_warnings(
            scramble_variables_rowwise(df, "nonexistent_column")
        )

        expect_length(w, 2)
        expect_match(w[1], "Some column names not found: nonexistent_column")
        expect_match(w[2], "Each column set must be a character vector, numeric positions, or tidyselect expression.")

    expect_warning(
        scramble_variables_rowwise(df, data.frame(a = 1)),
        "Failed to evaluate column set",
        fixed = FALSE
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
    result <- expect_warning(
        scramble_variables_rowwise(df,
                                   c("int_col1", "int_col2"),
                                   c("num_col1", "num_col2"),
                                   c("char_col1", "char_col2")
        ),
        NA
    )

    expect_s3_class(result, "data.frame")

    expect_type(result$int_col1, "integer")
    expect_type(result$int_col2, "integer")
    expect_type(result$num_col1, "double")
    expect_type(result$num_col2, "double")
    expect_type(result$char_col1, "character")
    expect_type(result$char_col2, "character")
    expect_s3_class(result$factor_col, "factor")
    expect_s3_class(result$date_col, "Date")
    expect_type(result$logical_col, "logical")

    expect_equal(result[c("factor_col", "date_col", "logical_col", "other")],
                 df[c("factor_col", "date_col", "logical_col", "other")])

    # Spot-check scrambling within sets
    expect_setequal(c(result$int_col1[1], result$int_col2[1]), c(1L, 4L))
    expect_setequal(c(result$num_col1[1], result$num_col2[1]), c(1.1, 4.4))
    expect_setequal(c(result$char_col1[1], result$char_col2[1]), c("a", "x"))
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
    expect_setequal(c(result2$day_1[1], result2$day_2[1], result2$day_3[1]), c(1, 2, 3))
    expect_setequal(c(result2$score_a[1], result2$score_b[1]), c(10, 20))
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

#  Warn on mixed types in one set
test_that("scramble_variables_rowwise warns on mixed types within a set", {
    df <- data.frame(
        num = c(1, 2, 3),
        char = c("a", "b", "c"),
        other = letters[4:6],
        stringsAsFactors = FALSE
    )

    expect_warning(
        result <- scramble_variables_rowwise(df, c("num", "char")),
        "Columns have mixed types: double, character. Scrambling may cause coercion.",
        fixed = TRUE
    )

    expect_type(result$num, "character")
    expect_type(result$char, "character")
    expect_setequal(c(result$num[1], result$char[1]), c("1", "a"))
    expect_setequal(c(result$num[2], result$char[2]), c("2", "b"))
    expect_setequal(c(result$num[3], result$char[3]), c("3", "c"))
    expect_equal(result$other, df$other)
})

#  Warn on mixed types in one of multiple sets
test_that("scramble_variables_rowwise warns on mixed types in one of multiple sets", {
    df <- data.frame(
        day_1 = c(1, 4),
        day_2 = c(2, 5),
        score_a = c(10, 40),
        score_b = c("X", "Y"),
        id = 1:2,
        stringsAsFactors = FALSE
    )

    expect_warning(
        result <- scramble_variables_rowwise(df,
                                             c("day_1", "day_2"),
                                             c("score_a", "score_b")
        ),
        "Columns have mixed types: double, character. Scrambling may cause coercion.",
        fixed = TRUE
    )

    expect_type(result$day_1, "double")
    expect_type(result$day_2, "double")
    expect_setequal(c(result$day_1[1], result$day_2[1]), c(1, 2))

    expect_type(result$score_a, "character")
    expect_type(result$score_b, "character")
    expect_setequal(c(result$score_a[1], result$score_b[1]), c("10", "X"))

    expect_equal(result$id, df$id)
})

#  Multiple warnings for multiple mixed-type sets
test_that("scramble_variables_rowwise warns for each mixed-type set", {
    df <- data.frame(
        a_num = c(1, 2),
        a_char = c("x", "y"),
        b_num = c(3, 4),
        b_char = c("z", "w"),
        stringsAsFactors = FALSE
    )

    w <- NULL
    result <- withCallingHandlers(
        scramble_variables_rowwise(df,
                                   c("a_num", "a_char"),
                                   c("b_num", "b_char")
        ),
        warning = function(warn) {
            w <<- c(w, conditionMessage(warn))
            invokeRestart("muffleWarning")
        }
    )

    expect_equal(length(w), 2)
    expect_true(all(grepl("Columns have mixed types", w)))

    expect_type(result$a_num, "character")
    expect_type(result$a_char, "character")
    expect_setequal(c(result$a_num[1], result$a_char[1]), c("1", "x"))

    expect_type(result$b_num, "character")
    expect_type(result$b_char, "character")
    expect_setequal(c(result$b_num[1], result$b_char[1]), c("3", "z"))
})
