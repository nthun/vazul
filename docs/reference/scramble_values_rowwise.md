# Scramble values rowwise in a data frame

For each row, shuffle the values across the selected columns. Columns
can be mixed types, but note that R may coerce types during the process
(function throws warning if mixed types detected).

## Usage

``` r
scramble_values_rowwise(data, cols)
```

## Arguments

- data:

  a data frame

- cols:

  Columns to scramble rowwise. Supports helpers like
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`where()`](https://tidyselect.r-lib.org/reference/where.html), etc.

## Value

A data frame with values scrambled rowwise across the selected columns.

## Examples

``` r
df <- data.frame(
  day_1 = c(1, 4, 7),
  day_2 = c(2, 5, 8),
  day_3 = c(3, 6, 9),
  id = 1:3
)

set.seed(123)
df |> scramble_values_rowwise(starts_with("day_"))
#>   day_1 day_2 day_3 id
#> 1     3     1     2  1
#> 2     5     4     6  2
#> 3     8     9     7  3
df |> scramble_values_rowwise(c("day_1", "day_2"))
#>   day_1 day_2 day_3 id
#> 1     1     2     3  1
#> 2     5     4     6  2
#> 3     7     8     9  3
```
