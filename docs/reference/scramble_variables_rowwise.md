# Scramble values across multiple columns rowwise in a data frame

For each row, shuffle values across the selected columns. All selections
passed via `...` are combined into a single set and scrambled together.
To scramble different variable groups separately, call the function
multiple times.

## Usage

``` r
scramble_variables_rowwise(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  Columns to scramble. All arguments are combined into a single set.
  Each can be:

  - Bare column names (e.g., `var1, var2`)

  - A tidyselect expression (e.g., `starts_with("day_")`)

  - A character vector of column names (e.g., `c("var1", "var2")`)

  If `data` is already a grouped `dplyr` data frame, existing grouping
  is ignored.

## Value

A data frame with values scrambled rowwise within the selected columns.

## Details

Rowwise scrambling moves values between columns, so selected columns
must be type-compatible. This function requires all selected columns to
have the same class (or be an integer/double mix). For factors, the
selected columns must also have identical levels.

## See also

[`scramble_values`](https://nthun.github.io/vazul/reference/scramble_values.md)
for scrambling a single vector, and
[`scramble_variables`](https://nthun.github.io/vazul/reference/scramble_variables.md)
for scrambling multiple variables.

## Examples

``` r
df <- data.frame(
  day_1 = c(1, 4, 7),
  day_2 = c(2, 5, 8),
  day_3 = c(3, 6, 9),
  score_a = c(10, 40, 70),
  score_b = c(20, 50, 80),
  id = 1:3
)

set.seed(123)
# Scramble one set of variables
library(dplyr)
df |> scramble_variables_rowwise(starts_with("day_"))
#>   day_1 day_2 day_3 score_a score_b id
#> 1     3     1     2      10      20  1
#> 2     5     4     6      40      50  2
#> 3     8     9     7      70      80  3

# Using character vectors
df |> scramble_variables_rowwise(c("day_1", "day_2", "day_3"))
#>   day_1 day_2 day_3 score_a score_b id
#> 1     1     2     3      10      20  1
#> 2     5     4     6      40      50  2
#> 3     9     8     7      70      80  3

# Scramble multiple sets separately
df |>
  scramble_variables_rowwise(starts_with("day_")) |>
  scramble_variables_rowwise(c("score_a", "score_b"))
#>   day_1 day_2 day_3 score_a score_b id
#> 1     3     1     2      10      20  1
#> 2     4     6     5      40      50  2
#> 3     9     8     7      70      80  3

# Multiple selectors are combined into one set (values can move between day_* and score_*)
df |> scramble_variables_rowwise(starts_with("day_"), starts_with("score_"))
#>   day_1 day_2 day_3 score_a score_b id
#> 1     1     3    10      20       2  1
#> 2     5     4     6      40      50  2
#> 3     8     9    70      80       7  3
```
