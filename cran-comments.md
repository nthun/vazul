## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies
There are currently no reverse dependencies for this package.

## Note on Breaking Changes
This release introduces a version bump (1.1.0) to reflect API refinements following formal peer review for a software journal. 
1. `mask_variables_rowwise()` was removed as it was deemed methodologically unsound for the intended use case of analysis blinding.
2. `scramble_variables_rowwise()` was internalized and consolidated into `scramble_variables(..., .byrow = TRUE)` to provide a more consistent 'tidy' interface.
3. Argument names were updated to use leading dots (e.g., `.groups`) to avoid conflicts with user data columns.
