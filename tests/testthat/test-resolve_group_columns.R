test_that("resolve_group_columns returns NULL when .groups is NULL", {
  df <- data.frame(x = 1:3, g = c("A", "A", "B"))

  expect_null(vazul:::resolve_group_columns(rlang::quo(NULL), df))
})

test_that("resolve_group_columns resolves group columns from symbols and character vectors", {
  df <- data.frame(x = 1:3, g1 = c("A", "A", "B"), g2 = c("M", "F", "M"))

  expect_equal(vazul:::resolve_group_columns(rlang::quo(g1), df), "g1")
  expect_equal(vazul:::resolve_group_columns(rlang::quo(c("g1", "g2")), df), c("g1", "g2"))
})

test_that("resolve_group_columns supports rlang::expr(...) wrapper for tidyselect", {
  df <- data.frame(x = 1:3, cat_a = c("A", "A", "B"), cat_b = c("M", "F", "M"))

  expect_equal(
    vazul:::resolve_group_columns(rlang::quo(rlang::expr(starts_with("cat"))), df),
    c("cat_a", "cat_b")
  )
})

test_that("resolve_group_columns errors on empty selection", {
  df <- data.frame(x = 1:3, g = c("A", "A", "B"))

  expect_error(
    vazul:::resolve_group_columns(rlang::quo(starts_with("nope")), df),
    "Error in .groups parameter: No columns selected for grouping.",
    fixed = TRUE
  )
})

test_that("resolve_group_columns errors on missing columns with a .groups-specific message", {
  df <- data.frame(x = 1:3, g = c("A", "A", "B"))

  expect_error(
    vazul:::resolve_group_columns(rlang::quo(does_not_exist), df),
    "Error in .groups parameter:",
    fixed = TRUE
  )
})

test_that("resolve_group_columns errors when group columns overlap with processed columns", {
  df <- data.frame(x = 1:3, g = c("A", "A", "B"))

  expect_error(
    vazul:::resolve_group_columns(rlang::quo(g), df, col_names = c("g", "x")),
    "Grouping columns cannot overlap with columns being processed.",
    fixed = TRUE
  )
})


