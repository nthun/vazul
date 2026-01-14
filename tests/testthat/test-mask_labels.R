# Tests for mask_labels function

test_that("mask_labels returns correct length and type with character vector", {
  # Test with character vector
  set.seed(123)
  x <- c("control", "treatment", "control", "treatment")
  result <- mask_labels(x)

  expect_equal(length(result), length(x))
  expect_type(result, "character")
  expect_true(all(grepl("^masked_group_", result)))
})

test_that("mask_labels works with factor vector", {
  # Test with factor vector
  set.seed(123)
  x <- factor(c("A", "B", "A", "B"))
  result <- mask_labels(x)

  expect_s3_class(result, "factor")
  expect_equal(length(result), length(x))
  expect_true(all(grepl("^masked_group_", as.character(result))))
})

test_that("mask_labels uses custom prefix correctly", {
  # Test with custom prefix
  set.seed(123)
  x <- c("group1", "group2", "group1")
  result <- mask_labels(x, prefix = "test_")

  expect_type(result, "character")
  expect_true(all(grepl("^test_", result)))
  expect_false(any(grepl("masked_group_", result)))
})

test_that("mask_labels produces random assignment", {
  # Test that the assignment is randomized
  x <- c("A", "B", "C", "D", "E")

  # Generate multiple results with different seeds
  set.seed(123)
  result1 <- mask_labels(x)

  set.seed(456)
  result2 <- mask_labels(x)

  # The masked labels should be different between runs
  # (with very high probability for 5 unique values)
  expect_false(identical(result1, result2))
})

test_that("mask_labels maintains mapping consistency within single call", {
  # Test that same original values get same masked labels within one call
  set.seed(123)
  x <- c("A", "B", "A", "C", "B", "A")
  result <- mask_labels(x)

  # Check that all "A"s get the same masked label
  a_labels <- result[x == "A"]
  expect_true(all(a_labels == a_labels[1]))

  # Check that all "B"s get the same masked label
  b_labels <- result[x == "B"]
  expect_true(all(b_labels == b_labels[1]))

  # Check that "A" and "B" get different labels
  expect_false(a_labels[1] == b_labels[1])
})

test_that("mask_labels handles single unique value", {
  # Test with vector containing only one unique value
  set.seed(123)
  x <- c("same", "same", "same")
  result <- mask_labels(x)

  expect_equal(length(result), 3)
  expect_true(all(result == "masked_group_01"))
  expect_type(result, "character")

  # Test with factor
  x_factor <- factor(c("same", "same", "same"))
  result_factor <- mask_labels(x_factor)
  expect_s3_class(result_factor, "factor")
  expect_true(all(as.character(result_factor) == "masked_group_01"))
})

test_that("mask_labels handles single element vector", {
  # Test with single element
  set.seed(123)
  x <- c("single")
  result <- mask_labels(x)

  expect_equal(length(result), 1)
  expect_equal(unname(result), "masked_group_01")
  expect_type(result, "character")
})

test_that("mask_labels produces consistent results with set.seed", {
  # Test deterministic behavior with same seed
  set.seed(42)
  x <- c("A", "B", "C")
  result1 <- mask_labels(x)

  set.seed(42)
  result2 <- mask_labels(x)

  expect_equal(result1, result2)
})

test_that("mask_labels creates correct number of unique labels", {
  # Test that number of unique masked labels equals number of unique inputs
  set.seed(123)
  x <- c("A", "B", "A", "C", "B", "D")
  result <- mask_labels(x)

  expect_equal(length(unique(result)), length(unique(x)))
  expect_equal(length(unique(result)), 4)
})

test_that("mask_labels validates input correctly", {
  # Test NULL input
  expect_error(
    mask_labels(NULL),
    "Input 'x' cannot be NULL. Please provide a vector.",
    fixed = TRUE
  )

  # Test non-vector input
  expect_error(
    mask_labels(data.frame(a = 1:3)),
    "Input 'x' must be a 1-dimensional vector",
    fixed = TRUE
  )

  # Test empty vector
  expect_error(
    mask_labels(character(0)),
    "Input 'x' cannot be an empty vector. Please provide a vector with at least one element.",
    fixed = TRUE
  )

  # Test with matrix (should fail as it's not a vector)
  expect_error(
    mask_labels(matrix(letters[1:6], nrow = 2)),
    "Input 'x' must be a 1-dimensional vector",
    fixed = TRUE
  )

  # Test non-character/non-factor input
  expect_error(
    mask_labels(1:5),
    "Input 'x' must be a character or factor vector. Received object of class: integer.",
    fixed = TRUE
  )

  expect_error(
    mask_labels(c(TRUE, FALSE, TRUE)),
    "Input 'x' must be a character or factor vector. Received object of class: logical.",
    fixed = TRUE
  )
})

test_that("mask_labels validates prefix parameter correctly", {
  x <- c("A", "B", "C")

  # Test NULL prefix
  expect_error(
    mask_labels(x, prefix = NULL),
    "Parameter 'prefix' cannot be NULL. Please provide a character string.",
    fixed = TRUE
  )

  # Test non-character prefix
  expect_error(
    mask_labels(x, prefix = 123),
    "Parameter 'prefix' must be a single character string.",
    fixed = TRUE
  )

  # Test multiple string prefix
  expect_error(
    mask_labels(x, prefix = c("prefix1", "prefix2")),
    "Parameter 'prefix' must be a single character string.",
    fixed = TRUE
  )

  # Test empty string prefix
  expect_error(
    mask_labels(x, prefix = ""),
    "Parameter 'prefix' cannot be an empty string. Please provide a ",
    "non-empty character string.",
    fixed = TRUE
  )
})

test_that("mask_labels preserves factor levels correctly", {
  # Test that factor output has correct levels
  set.seed(123)
  x <- factor(c("low", "high", "medium", "low"), levels = c("low", "medium", "high"))
  result <- mask_labels(x)

  expect_s3_class(result, "factor")
  expect_equal(length(levels(result)), 3)  # Should have 3 unique masked labels
  expect_true(all(grepl("^masked_group_", levels(result))))
})

test_that("mask_labels works with special characters and spaces", {
  # Test with special characters in values
  set.seed(123)
  x <- c("group-1", "group 2", "group_3", "group-1")
  result <- mask_labels(x)

  expect_equal(length(result), 4)
  expect_equal(length(unique(result)), 3)  # 3 unique original values
  expect_type(result, "character")
})

test_that("mask_labels handles large vectors efficiently", {
  # Test with larger vector
  set.seed(123)
  x <- sample(letters[1:5], 1000, replace = TRUE)
  result <- mask_labels(x)

  expect_equal(length(result), 1000)
  expect_equal(length(unique(result)), 5)  # 5 unique original values
  expect_true(all(grepl("^masked_group_", result)))
})

test_that("mask_labels handles NA values correctly", {
  # Test with NA values
  set.seed(123)
  x <- c("A", "B", NA, "A", "B", NA)
  result <- mask_labels(x)

  expect_equal(length(result), 6)
  expect_equal(sum(is.na(result)), 2)  # Should preserve NA positions
  expect_equal(length(unique(result[!is.na(result)])), 2)  # 2 non-NA unique values
})

