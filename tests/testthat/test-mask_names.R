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

test_that("mask_names supports tidyselect range without warnings", {
  set.seed(123)
  expect_warning(
    result <- mask_names(iris, Sepal.Length:Petal.Width, prefix = "a"),
    NA
  )

  masked_names <- names(result)
  expect_equal(sum(startsWith(masked_names, "a")), 4)
  expect_true("Species" %in% masked_names)
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

  # Test data frame with duplicate column names
  df_dup <- data.frame(var1 = 1:3, var1 = 4:6, var2 = 7:9, check.names = FALSE)
  expect_error(
    mask_names(df_dup, c("var2"), prefix = "x_"),
    "Input 'data' must have unique column names."
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

test_that("mask_names randomizes column-to-label assignment", {
  df <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6),
    var3 = c(7, 8, 9)
  )

  set.seed(123)
  result1 <- mask_names(df, c("var1", "var2", "var3"), prefix = "x_")

  set.seed(456)
  result2 <- mask_names(df, c("var1", "var2", "var3"), prefix = "x_")

  # After the column sort, masked names are always in alphabetical order —
  # the randomisation is in which original column ends up under each label.
  expect_equal(names(result1), names(result2))
  expect_false(identical(
    unname(unlist(result1[startsWith(names(result1), "x_")])),
    unname(unlist(result2[startsWith(names(result2), "x_")]))
  ))
})

# Tests for keep_suffixes parameter

test_that("keep_suffixes preserves a single suffix on matching columns", {
  df <- data.frame(
    treat_1   = c(1, 2, 3),
    treat_1_r = c(3, 2, 1),
    id = 1:3
  )

  set.seed(42)
  result <- mask_names(df, starts_with("treat_"), prefix = "A_",
                       keep_suffixes = "_r")

  masked <- names(result)[names(result) != "id"]
  expect_equal(length(masked), 2)
  expect_equal(sum(endsWith(masked, "_r")), 1)
  expect_equal(sum(!endsWith(masked, "_r")), 1)
  # Data of treat_1_r is under the _r-suffixed label
  r_col <- masked[endsWith(masked, "_r")]
  expect_equal(result[[r_col]], df$treat_1_r)
})

test_that("keep_suffixes handles multiple suffixes", {
  df <- data.frame(
    treat_1   = c(1, 2, 3),
    treat_1_r = c(3, 2, 1),
    treat_1_z = c(0, 0, 0),
    id = 1:3
  )

  set.seed(42)
  result <- mask_names(df, starts_with("treat_"), prefix = "A_",
                       keep_suffixes = c("_r", "_z"))

  masked <- names(result)[names(result) != "id"]
  expect_equal(sum(endsWith(masked, "_r")), 1)
  expect_equal(sum(endsWith(masked, "_z")), 1)
  expect_equal(sum(!endsWith(masked, "_r") & !endsWith(masked, "_z")), 1)
})

test_that("keep_suffixes does not add suffix to non-matching columns", {
  df <- data.frame(var1 = 1:3, var2 = 4:6)

  set.seed(42)
  result <- mask_names(df, c("var1", "var2"), prefix = "X_",
                       keep_suffixes = "_r")

  expect_true(all(grepl("^X_\\d{2}$", names(result))))
})

test_that("keep_suffixes longest match wins", {
  df <- data.frame(v_score_r = 1:3, v_r = 4:6)

  set.seed(42)
  result <- suppressWarnings(
    mask_names(df, c("v_score_r", "v_r"), prefix = "A_",
               keep_suffixes = c("_r", "_score_r"))
  )

  masked <- names(result)
  expect_equal(sum(endsWith(masked, "_score_r")), 1)
  expect_equal(sum(endsWith(masked, "_r") & !endsWith(masked, "_score_r")), 1)
  score_r_col <- masked[endsWith(masked, "_score_r")]
  expect_equal(result[[score_r_col]], df$v_score_r)
})

test_that("keep_suffixes warns when multiple suffixes overlap on a column", {
  df <- data.frame(v_score_r = 1:3, v_r = 4:6)

  set.seed(42)
  expect_warning(
    mask_names(df, c("v_score_r", "v_r"), prefix = "A_",
               keep_suffixes = c("_r", "_score_r")),
    "v_score_r.*_r.*_score_r"
  )
})

test_that("keep_suffixes does not warn when suffixes don't overlap", {
  df <- data.frame(v_score_r = 1:3, v_z = 4:6)

  set.seed(42)
  expect_no_warning(
    mask_names(df, c("v_score_r", "v_z"), prefix = "A_",
               keep_suffixes = c("_r", "_z"))
  )
})

test_that("keep_suffixes duplicate entries are silently deduped", {
  df <- data.frame(treat_1 = 1:3, treat_1_r = 4:6)

  set.seed(42)
  result_single <- mask_names(df, c("treat_1", "treat_1_r"), prefix = "A_",
                               keep_suffixes = "_r")
  set.seed(42)
  result_dup    <- mask_names(df, c("treat_1", "treat_1_r"), prefix = "A_",
                               keep_suffixes = c("_r", "_r"))

  expect_equal(names(result_single), names(result_dup))
})

test_that("keep_suffixes NULL (default) is identical to omitting it", {
  df <- data.frame(var1 = 1:3, var2 = 4:6)

  set.seed(42)
  result_default <- mask_names(df, c("var1", "var2"), prefix = "X_")
  set.seed(42)
  result_null    <- mask_names(df, c("var1", "var2"), prefix = "X_",
                                keep_suffixes = NULL)

  expect_equal(names(result_default), names(result_null))
})

test_that("keep_suffixes collision detection uses suffixed masked names", {
  # With one column to mask, the base name is A_01; appending _r gives A_01_r.
  # If A_01_r already exists in the data frame, it should error.
  df <- data.frame(treat_1_r = 1:3, A_01_r = 4:6)

  set.seed(1)
  expect_error(
    mask_names(df, "treat_1_r", prefix = "A_", keep_suffixes = "_r"),
    "Name collision detected"
  )
})

test_that("keep_suffixes suffix never lands on wrong column data", {
  df <- data.frame(treat_01 = 1:3, treat_01_r = c(3L, 2L, 1L))

  for (s in 1:20) {
    set.seed(s)
    result <- mask_names(df, c("treat_01", "treat_01_r"),
                         prefix = "A_", keep_suffixes = "_r")
    r_col <- names(result)[endsWith(names(result), "_r")]
    expect_equal(length(r_col), 1)
    expect_equal(result[[r_col]], df$treat_01_r)
  }
})

test_that("keep_suffixes validates invalid inputs", {
  df <- data.frame(var1 = 1:3)

  expect_error(
    mask_names(df, "var1", prefix = "X_", keep_suffixes = 123),
    "must be a non-empty character vector or NULL"
  )
  expect_error(
    mask_names(df, "var1", prefix = "X_", keep_suffixes = ""),
    "must not contain empty strings"
  )
  expect_error(
    mask_names(df, "var1", prefix = "X_", keep_suffixes = character(0)),
    "must be a non-empty character vector or NULL"
  )
})
