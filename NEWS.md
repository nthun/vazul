# vazul 0.5.0
## 2025-12-02

* Now all main functions use the `...` for accepting tidyselect helpers (BREAKING CHANGE!)
* Update documentation
* Add vignette for main functions
* Removed `mask_labels_rowwise()` and `scramble_values_rowwise()` functions as they were confusing and unnecessary.
* Updated the README file to reflect the changes.

# vazul 0.4.0
## 2025-09-29

* Add `together` parameter to `scramble_variables()` function.
* Update documentation
* Add new tests

# vazul 0.3.0
## 2025-09-25

* Add `mask_labels()`, `mask_variables()`, `mask_variables_rowwise()`, `mask_names()` functions to mask factor levels and variable names.
* Add unit tests for all new functions
* Add lifcycle badge to README

# vazul 0.2.1
## 2025-09-19
* Add rowwise functions with unit tests
* Add a devel branch to do development from now on


# vazul 0.1.0
## 2025-09-18
* `scramble_values()` can now use tidyselect helpers
* Improved unit tests
* Add imports: {tidyselect}, {utils}

# vazul 0.0.1
## 2025-09-15
* Unit tests added using `testthat`.
* Add error handling to `scramble_variables()`
* Add imports: {dplyr}, {rlang}
## 2025-09-09
* Add `scramble_values()` and `scramble_variables()` functions.
* Build package using `pkgdown::build_site()`.
* Add README

# vazul 0.0.0.9000
## 2025-08-29
* Package skeleton created with `usethis::create_package()`.
* Add Williams et. al dataset.
* Add MARP dataset.
