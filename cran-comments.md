## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies
There are currently no reverse dependencies for this package.

## Summary of changes in this version (1.2.0)
This release adds a new feature and a bug fix; it contains no breaking changes.

1. `mask_names()` gains a `keep_suffixes` argument to preserve a fixed suffix (e.g. `_r` for
   reverse-scored items) verbatim in masked names instead of masking it away.
2. `mask_names()` now sorts masked columns alphabetically by their masked name within their
   original positions, so column position no longer leaks which original variable a masked
   name refers to.
3. Fixed a bug in `scramble_variables()` where a `.groups` column positioned before or between
   target columns could cause the wrong column to be scrambled (index-based column selection
   replaced with name-based selection).
4. `mask_names()`, `mask_variables()`, and `scramble_variables()` now reject data frames with
   duplicate column names with a clear error.
5. Corrected the source citations for the `williams` and `marp` example datasets.
