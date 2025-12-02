# Scramble multiple column sets rowwise in a data frame

For each row, independently shuffle values within each group of selected
columns.

## Usage

``` r
scramble_variables_rowwise(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  One or more column sets. Each can be:

  - A tidyselect expression (e.g., `starts_with("day_")`)

  - A character vector of column names (e.g., `c("day_1", "day_2")`)

  - Multiple sets can be provided as separate arguments

## Value

A data frame with values scrambled rowwise within each selected column
set.

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
df |> scramble_variables_rowwise(starts_with("day_"))
#>   day_1 day_2 day_3 score_a score_b id
#> 1     3     1     2      10      20  1
#> 2     5     4     6      40      50  2
#> 3     8     9     7      70      80  3
df |> scramble_variables_rowwise(c("day_1", "day_2"))
#>   day_1 day_2 day_3 score_a score_b id
#> 1     1     2     3      10      20  1
#> 2     5     4     6      40      50  2
#> 3     7     8     9      70      80  3
df |> scramble_variables_rowwise(
  starts_with("day_"),
  c("score_a", "score_b")
)
#>   day_1 day_2 day_3 score_a score_b id
#> 1     3     2     1      10      20  1
#> 2     6     4     5      50      40  2
#> 3     7     9     8      80      70  3
```
