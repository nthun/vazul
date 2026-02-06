# Changelog

## vazul 1.1.0

### 2026-02-06

- Breaking Change: Removed `mask_variables_rowwise()` as row-wise
  masking is methodologically inconsistent with analysis blinding.
- Breaking Change: `scramble_variables_rowwise()` is now internal. Its
  functionality has been consolidated into
  `scramble_variables(..., .byrow = TRUE)`.
- API Improvement: Arguments in
  [`scramble_variables()`](https://nthun.github.io/vazul/reference/scramble_variables.md)
  and
  [`mask_variables()`](https://nthun.github.io/vazul/reference/mask_variables.md)
  parameters now use leading dots (e.g., .groups, .together) to avoid
  naming conflicts with data columns.
- Update documentation, vignettes, and website to reflect changes in
  function names and parameters.

## vazul 1.0.0

CRAN release: 2026-01-28

### 2026-01-16

- Finalize functions
- Streamline validation and generics
- Unify the API interface for functions
- Add tests, change vignettes to match functionality
- Fix codecov

## vazul 0.4.0

### 2025-09-29

- Add `together` parameter to
  [`scramble_variables()`](https://nthun.github.io/vazul/reference/scramble_variables.md)
  function.
- Update documentation
- Add new tests

## vazul 0.3.0

### 2025-09-25

- Add
  [`mask_labels()`](https://nthun.github.io/vazul/reference/mask_labels.md),
  [`mask_variables()`](https://nthun.github.io/vazul/reference/mask_variables.md),
  `mask_variables_rowwise()`,
  [`mask_names()`](https://nthun.github.io/vazul/reference/mask_names.md)
  functions to mask factor levels and variable names.
- Add unit tests for all new functions
- Add lifecycle badge to README

## vazul 0.2.1

### 2025-09-19

- Add rowwise functions with unit tests
- Add a devel branch to do development from now on

## vazul 0.1.0

### 2025-09-18

- [`scramble_values()`](https://nthun.github.io/vazul/reference/scramble_values.md)
  can now use tidyselect helpers
- Improved unit tests
- Add imports: {tidyselect}, {utils}

## vazul 0.0.1

### 2025-09-15

- Unit tests added using `testthat`.
- Add error handling to
  [`scramble_variables()`](https://nthun.github.io/vazul/reference/scramble_variables.md)
- Add imports: {dplyr}, {rlang} \## 2025-09-09
- Add
  [`scramble_values()`](https://nthun.github.io/vazul/reference/scramble_values.md)
  and
  [`scramble_variables()`](https://nthun.github.io/vazul/reference/scramble_variables.md)
  functions.
- Build package using
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html).
- Add README

## vazul 0.0.0.9000

### 2025-08-29

- Package skeleton created with
  [`usethis::create_package()`](https://usethis.r-lib.org/reference/create_package.html).
- Add Williams et. al dataset.
- Add MARP dataset.
