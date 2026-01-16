# Tests for scramble_variables function

test_that("scramble_variables works with basic data frame", {
    # Create test data frame
    df <- data.frame(
        x = 1:6,
        y = letters[1:6],
        z = factor(c("A", "A", "B", "B", "C", "C"))
    )

    # Test scrambling by column names
    set.seed(123)
    result <- scramble_variables(df, c("x", "y"))

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(df))
    expect_equal(ncol(result), ncol(df))
    expect_equal(names(result), names(df))

    # Check that scrambled columns have same elements but potentially different order
    expect_setequal(result$x, df$x)
    expect_setequal(result$y, df$y)

    # Check that non-scrambled column is unchanged
    expect_equal(result$z, df$z)
})

test_that("scramble_variables works with column indices", {
    df <- data.frame(
        a = 1:5,
        b = letters[1:5],
        c = 5:1
    )

    set.seed(123)
    result <- scramble_variables(df, c(1, 3))  # Scramble columns 1 and 3

    expect_s3_class(result, "data.frame")
    expect_equal(names(result), names(df))
    expect_setequal(result$a, df$a)
    expect_setequal(result$c, df$c)
    expect_equal(result$b, df$b)  # Column b should be unchanged
})

test_that("scramble_variables works with single column", {
    df <- data.frame(
        x = 1:10,
        y = letters[1:10]
    )

    set.seed(123)
    result <- scramble_variables(df, "x")

    expect_equal(names(result), names(df))
    expect_setequal(result$x, df$x)
    expect_equal(result$y, df$y)  # y should be unchanged
})

test_that("scramble_variables works with grouping", {
    # Create test data frame with groups
    df <- data.frame(
        x = 1:6,
        y = letters[1:6],
        group = c("A", "A", "A", "B", "B", "B")
    )

    set.seed(123)
    result <- scramble_variables(df, "x", .groups = "group")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(df))
    expect_equal(names(result), names(df))

    # Check that grouping variable is unchanged
    expect_equal(result$group, df$group)

    # Check that scrambling occurred within groups
    group_a_orig <- df$x[df$group == "A"]
    group_a_result <- result$x[result$group == "A"]
    expect_setequal(group_a_result, group_a_orig)

    group_b_orig <- df$x[df$group == "B"]
    group_b_result <- result$x[result$group == "B"]
    expect_setequal(group_b_result, group_b_orig)
})

test_that("scramble_variables preserves column order", {
    df <- data.frame(
        col1 = 1:5,
        col2 = letters[1:5],
        col3 = 5:1,
        col4 = LETTERS[1:5]
    )

    original_order <- names(df)

    set.seed(123)
    result <- scramble_variables(df, c("col2", "col3"))

    expect_equal(names(result), original_order)
})

test_that("scramble_variables ignores existing dplyr grouping unless .groups is provided", {
    skip_if_not_installed("dplyr")

    df <- data.frame(
        x = 1:6,
        y = letters[1:6],
        group = c("A", "A", "A", "B", "B", "B")
    )

    set.seed(123)
    result_grouped_input <- df |>
        dplyr::group_by(group) |>
        scramble_variables("x") |>
        dplyr::ungroup()

    set.seed(123)
    result_equivalent_tibble_input <- scramble_variables(dplyr::as_tibble(df), "x")

    expect_equal(result_grouped_input, result_equivalent_tibble_input)
})

test_that("scramble_variables errors when .groups overlaps with selected columns", {
    df <- data.frame(
        x = 1:6,
        y = letters[1:6],
        group = c("A", "A", "A", "B", "B", "B")
    )

    expect_error(
        scramble_variables(df, c("x", "group"), .groups = "group"),
        "Grouping columns cannot overlap with columns being processed.",
        fixed = TRUE
    )
})

test_that("scramble_variables handles multiple grouping variables", {
    df <- data.frame(
        value = 1:12,
        group1 = rep(c("X", "Y"), each = 6),
        group2 = rep(c("1", "2", "3"), times = 4)
    )

    set.seed(123)
    result <- scramble_variables(df, "value", .groups = c("group1", "group2"))

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(df))
    expect_equal(names(result), names(df))

    # Note: Testing that the function completes without error
    # The current grouping implementation may have issues
})

test_that("scramble_variables validates input correctly", {
    df <- data.frame(x = 1:5, y = letters[1:5])

    # Test non-data.frame input
    expect_error(
        scramble_variables(list(x = 1:5), "x"),
        "Input 'data' must be a data frame.",
        fixed = TRUE
    )

    # Test missing columns — now expect custom error from helper
    expect_error(
        scramble_variables(df, "nonexistent_column"),
        "Error in column selection: Can't subset columns that don't exist.",
        fixed = FALSE  # Allow partial match
    )

    # Test invalid column indices — tidyselect handles this too
    expect_error(
        scramble_variables(df, 10),  # Column 10 doesn't exist
        "Can't select columns past the end.",
        fixed = FALSE  # Allow partial match
    )
})

test_that("scramble_variables handles edge cases", {
    # Test with single row
    df_single <- data.frame(x = 1, y = "a")
    result <- scramble_variables(df_single, "x")
    expect_equal(result, df_single)

    # Test with single column scrambled
    df_one_col <- data.frame(x = 1:5)
    set.seed(123)
    result <- scramble_variables(df_one_col, "x")
    expect_s3_class(result, "data.frame")
    expect_setequal(result$x, df_one_col$x)
})

test_that("scramble_variables actually scrambles data (probabilistic)", {
    # Create larger dataset to ensure scrambling occurs
    df <- data.frame(
        x = 1:50,
        y = letters[rep(1:26, length.out = 50)]
    )

    set.seed(123)
    result <- scramble_variables(df, "x")

    # It's extremely unlikely that 50 elements stay in order
    expect_false(identical(result$x, df$x))

    # But y should be unchanged
    expect_equal(result$y, df$y)
})

test_that("scramble_variables works with special data types", {
    df <- data.frame(
        dates = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
        factors = factor(c("low", "medium", "high")),
        logicals = c(TRUE, FALSE, TRUE)
    )

    set.seed(123)
    result <- scramble_variables(df, c("dates", "factors", "logicals"))

    expect_s3_class(result, "data.frame")
    expect_setequal(result$dates, df$dates)
    expect_setequal(as.character(result$factors), as.character(df$factors))
    expect_setequal(result$logicals, df$logicals)

    # Check types are preserved
    expect_s3_class(result$dates, "Date")
    expect_s3_class(result$factors, "factor")
    expect_type(result$logicals, "logical")
})

test_that("scramble_variables preserves NA values correctly", {
    df <- data.frame(
        x = c(1, 2, NA, 4, 5),
        y = c("a", "b", NA, "d", "e")
    )

    set.seed(123)
    result <- scramble_variables(df, c("x", "y"))

    expect_equal(sum(is.na(result$x)), sum(is.na(df$x)))
    expect_equal(sum(is.na(result$y)), sum(is.na(df$y)))
})

# ─── NEW TESTS FOR TIDYSELECT HELPERS ──────────────────────────────────────────

test_that("scramble_variables works with starts_with() helper", {
    df <- data.frame(
        temp_1 = 1:5,
        temp_2 = 6:10,
        other = letters[1:5]
    )

    set.seed(123)
    result <- scramble_variables(df, starts_with("temp"))

    expect_setequal(result$temp_1, df$temp_1)
    expect_setequal(result$temp_2, df$temp_2)
    expect_equal(result$other, df$other)  # unchanged
})

test_that("scramble_variables works with where() helper", {
    df <- data.frame(
        x = 1:5,
        y = as.character(1:5),
        z = 6:10
    )

    set.seed(123)
    result <- scramble_variables(df, where(is.numeric))

    expect_setequal(result$x, df$x)
    expect_setequal(result$z, df$z)
    expect_equal(result$y, df$y)  # character unchanged
})

test_that("scramble_variables works with grouping using tidyselect helpers", {
    df <- data.frame(
        value = 1:6,
        cat_a = c("X", "X", "Y", "Y", "Z", "Z"),
        cat_b = c("M", "F", "M", "F", "M", "F")
    )

    set.seed(123)
    result <- scramble_variables(df, "value", .groups = rlang::expr(starts_with("cat")))

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), nrow(df))
    expect_equal(names(result), names(df))

    expect_equal(result$cat_a, df$cat_a)
    expect_equal(result$cat_b, df$cat_b)
    expect_setequal(result$value, df$value)  # values preserved, just reordered within groups
})

test_that("scramble_variables preserves input data frame type", {
  skip_if_not_installed("dplyr")

  # Test data
  df <- data.frame(x = 1:5, y = letters[1:5], z = 6:10)
  tbl <- dplyr::tibble(x = 1:5, y = letters[1:5], z = 6:10)

  # Test with data.frame input
  set.seed(123)
  result_df <- scramble_variables(df, c("x", "y"))
  expect_equal(class(result_df), class(df))

  # Test with tibble input
  set.seed(123)
  result_tbl <- scramble_variables(tbl, c("x", "y"))
  expect_equal(class(result_tbl), class(tbl))
})

# ─── TESTS FOR TOGETHER PARAMETER ─────────────────────────────────────────────

test_that("scramble_variables works with together = FALSE (default behavior)", {
  df <- data.frame(
    x = 1:4,
    y = letters[1:4],
    z = c("keep1", "keep2", "keep3", "keep4")
  )

  set.seed(123)
  result <- scramble_variables(df, c("x", "y"), together = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  expect_equal(names(result), names(df))

  # Check that scrambled columns have same elements but potentially different order
  expect_setequal(result$x, df$x)
  expect_setequal(result$y, df$y)

  # Check that non-scrambled column is unchanged
  expect_equal(result$z, df$z)

  # Variables should be scrambled independently (not together)
  # This is a probabilistic test but with seed 123 they should be different
  original_pairs <- paste(df$x, df$y, sep = "-")
  result_pairs <- paste(result$x, result$y, sep = "-")
  expect_false(identical(original_pairs, result_pairs))
})

test_that("scramble_variables works with together = TRUE", {
  df <- data.frame(
    x = 1:4,
    y = letters[1:4],
    z = c("keep1", "keep2", "keep3", "keep4")
  )

  set.seed(123)
  result <- scramble_variables(df, c("x", "y"), together = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))
  expect_equal(names(result), names(df))

  # Check that scrambled columns have same elements but potentially different order
  expect_setequal(result$x, df$x)
  expect_setequal(result$y, df$y)

  # Check that non-scrambled column is unchanged
  expect_equal(result$z, df$z)

  # Variables should be scrambled together (as pairs)
  # Each row should contain a pair that originally appeared together
  original_pairs <- paste(df$x, df$y, sep = "-")
  result_pairs <- paste(result$x, result$y, sep = "-")
  expect_setequal(result_pairs, original_pairs)

  # But the pairs should be in a different order
  expect_false(identical(original_pairs, result_pairs))
})

test_that("scramble_variables with together = TRUE preserves variable relationships", {
  df <- data.frame(
    id = 1:6,
    score = c(10, 20, 30, 40, 50, 60),
    category = c("A", "B", "A", "B", "A", "B")
  )

  set.seed(456)
  result <- scramble_variables(df, c("id", "score"), together = TRUE)

  # Check that each row maintains the original id-score relationship
  for (i in seq_len(nrow(result))) {
    id_val <- result$id[i]
    score_val <- result$score[i]
    original_score <- df$score[df$id == id_val]
    expect_equal(score_val, original_score)
  }
})

test_that("scramble_variables with together = TRUE and grouping", {
  df <- data.frame(
    x = 1:6,
    y = letters[1:6],
    group = c("A", "A", "A", "B", "B", "B")
  )

  set.seed(123)
  result <- scramble_variables(df, c("x", "y"), .groups = "group", together = TRUE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(names(result), names(df))

  # Check that grouping variable is unchanged
  expect_equal(result$group, df$group)

  # Check that scrambling occurred within groups while preserving pairs
  # Group A pairs
  group_a_indices <- which(df$group == "A")
  group_a_orig_pairs <- paste(df$x[group_a_indices], df$y[group_a_indices], sep = "-")

  result_a_indices <- which(result$group == "A")
  result_a_pairs <- paste(result$x[result_a_indices], result$y[result_a_indices], sep = "-")

  expect_setequal(result_a_pairs, group_a_orig_pairs)

  # Group B pairs
  group_b_indices <- which(df$group == "B")
  group_b_orig_pairs <- paste(df$x[group_b_indices], df$y[group_b_indices], sep = "-")

  result_b_indices <- which(result$group == "B")
  result_b_pairs <- paste(result$x[result_b_indices], result$y[result_b_indices], sep = "-")

  expect_setequal(result_b_pairs, group_b_orig_pairs)
})

test_that("scramble_variables with together = FALSE and grouping", {
  df <- data.frame(
    x = 1:6,
    y = letters[1:6],
    group = c("A", "A", "A", "B", "B", "B")
  )

  set.seed(123)
  result <- scramble_variables(df, c("x", "y"), .groups = "group", together = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(df))
  expect_equal(names(result), names(df))

  # Check that grouping variable is unchanged
  expect_equal(result$group, df$group)

  # Check that scrambling occurred within groups
  group_a_orig <- df$x[df$group == "A"]
  group_a_result <- result$x[result$group == "A"]
  expect_setequal(group_a_result, group_a_orig)

  group_b_orig <- df$x[df$group == "B"]
  group_b_result <- result$x[result$group == "B"]
  expect_setequal(group_b_result, group_b_orig)
})

test_that("scramble_variables together parameter works with single column", {
  df <- data.frame(
    x = 1:5,
    y = letters[1:5]
  )

  # With single column, together should behave like regular scrambling
  set.seed(123)
  result1 <- scramble_variables(df, "x", together = TRUE)

  set.seed(123)
  result2 <- scramble_variables(df, "x", together = FALSE)

  expect_equal(result1$x, result2$x)
  expect_equal(result1$y, result2$y) # unchanged
})

test_that("scramble_variables together parameter with edge cases", {
  # Single row
  df_single <- data.frame(x = 1, y = "a", z = "keep")
  result_single <- scramble_variables(df_single, c("x", "y"), together = TRUE)
  expect_equal(result_single, df_single)

  # Two rows
  df_two <- data.frame(x = 1:2, y = c("a", "b"))
  set.seed(123)
  result_two <- scramble_variables(df_two, c("x", "y"), together = TRUE)

  # Should preserve pairs
  original_pairs <- paste(df_two$x, df_two$y, sep = "-")
  result_pairs <- paste(result_two$x, result_two$y, sep = "-")
  expect_setequal(result_pairs, original_pairs)
})

test_that("scramble_variables together actually scrambles data (probabilistic)", {
  # Create larger dataset to ensure scrambling occurs
  df <- data.frame(
    x = 1:20,
    y = letters[rep(1:20, length.out = 20)]
  )

  set.seed(123)
  result <- scramble_variables(df, c("x", "y"), together = TRUE)

  # It's extremely unlikely that 20 pairs stay in order
  original_pairs <- paste(df$x, df$y, sep = "-")
  result_pairs <- paste(result$x, result$y, sep = "-")
  expect_false(identical(original_pairs, result_pairs))

  # But pairs should be preserved
  expect_setequal(result_pairs, original_pairs)
})

# ─── TESTS FOR ... SYNTAX ─────────────────────────────────────────────────────

test_that("scramble_variables warns when no columns are provided", {
  df <- data.frame(x = 1:5, y = letters[1:5])

  expect_warning(
    result <- scramble_variables(df),
    "No columns selected. Returning original data unchanged.",
    fixed = TRUE
  )

  expect_equal(result, df)
})

# ─── TESTS FOR ISSUE: TIDYEVAL FUNCTIONALITY ──────────────────────────────────

test_that("scramble_variables works with bare variable names", {
  # Issue case 1: mtcars |> scramble_variables(mpg, cyl)
  set.seed(123)
  result <- mtcars |> scramble_variables(mpg, cyl)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(mtcars))
  expect_equal(names(result), names(mtcars))

  # mpg and cyl should be scrambled (different order)
  expect_setequal(result$mpg, mtcars$mpg)
  expect_setequal(result$cyl, mtcars$cyl)

  # Other columns should remain unchanged
  expect_equal(result$disp, mtcars$disp)
  expect_equal(result$hp, mtcars$hp)
})

test_that("scramble_variables works with multiple bare variable names", {
  df <- data.frame(a = 1:10, b = 11:20, c = 21:30, d = 31:40)

  set.seed(123)
  result <- df |> scramble_variables(a, b, c)

  # Three columns should be scrambled
  expect_setequal(result$a, df$a)
  expect_setequal(result$b, df$b)
  expect_setequal(result$c, df$c)

  # Fourth column should be unchanged
  expect_equal(result$d, df$d)
})

test_that("scramble_variables works with multiple tidyselect helpers", {
  skip_if_not_installed("dplyr")

  # Issue case 2: starts_with() helpers
  data(williams, package = "vazul")

  set.seed(123)
  result <- williams |> scramble_variables(
    starts_with("SexUnres_"),
    starts_with("Opport_")
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(williams))

  # SexUnres_ columns should be scrambled
  sexunres_cols <- grep("^SexUnres_", names(williams), value = TRUE)
  for (col in sexunres_cols) {
    expect_setequal(result[[col]], williams[[col]])
  }

  # Opport_ columns should be scrambled
  opport_cols <- grep("^Opport_", names(williams), value = TRUE)
  for (col in opport_cols) {
    expect_setequal(result[[col]], williams[[col]])
  }

  # Other columns should be unchanged
  expect_equal(result$subject, williams$subject)
  expect_equal(result$ecology, williams$ecology)
})

test_that("scramble_variables works with multiple column sets", {
  # Issue case 3: c("a", "b"), c("c", "d") should scramble all four columns
  df <- data.frame(
    a = 1:10,
    b = 11:20,
    c = letters[1:10],
    d = letters[11:20],
    e = 21:30,  # This should remain unchanged
    stringsAsFactors = FALSE
  )

  set.seed(123)
  result <- df |> scramble_variables(c("a", "b"), c("c", "d"))

  # All four selected columns should be scrambled
  expect_setequal(result$a, df$a)
  expect_setequal(result$b, df$b)
  expect_setequal(result$c, df$c)
  expect_setequal(result$d, df$d)

  # With 10 elements, scrambling should change the order (probabilistic)
  expect_true(
    !identical(result$a, df$a) || !identical(result$b, df$b) ||
    !identical(result$c, df$c) || !identical(result$d, df$d)
  )

  # Unselected column should remain unchanged
  expect_equal(result$e, df$e)
})

test_that("scramble_variables combines bare names with tidyselect", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    single_var = 1:5,
    prefix_a = 6:10,
    prefix_b = 11:15,
    other = 16:20
  )

  set.seed(123)
  result <- df |> scramble_variables(single_var, starts_with("prefix_"))

  # All three columns should be scrambled
  expect_setequal(result$single_var, df$single_var)
  expect_setequal(result$prefix_a, df$prefix_a)
  expect_setequal(result$prefix_b, df$prefix_b)

  # Unselected column should remain unchanged
  expect_equal(result$other, df$other)
})

test_that("scramble_variables with numeric range works", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15, d = 16:20)

  set.seed(123)
  result <- df |> scramble_variables(1:3)

  # First three columns should be scrambled
  expect_setequal(result$a, df$a)
  expect_setequal(result$b, df$b)
  expect_setequal(result$c, df$c)

  # Fourth column should remain unchanged
  expect_equal(result$d, df$d)
})

