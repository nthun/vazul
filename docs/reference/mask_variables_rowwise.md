# Mask categorical labels across multiple column sets rowwise in a data frame

For each row, independently mask labels within each group of selected
columns.

## Usage

``` r
mask_variables_rowwise(data, ..., prefix = "masked_group_")
```

## Arguments

- data:

  A data frame.

- ...:

  One or more column sets. Each can be:

  - A tidyselect expression (e.g., `starts_with("treat_")`)

  - A character vector of column names (e.g., `c("treat_1", "treat_2")`)

  - Multiple sets can be provided as separate arguments

- prefix:

  character string to use as prefix for masked labels. Default is
  "masked_group\_"

## Value

A data frame with labels masked rowwise within each selected column set.

## Examples

``` r
df <- data.frame(
  treat_1 = c("control", "treatment", "placebo"),
  treat_2 = c("treatment", "placebo", "control"),
  treat_3 = c("placebo", "control", "treatment"),
  condition_a = c("A", "B", "A"),
  condition_b = c("B", "A", "B"),
  id = 1:3
)

set.seed(123)
df |> mask_variables_rowwise(starts_with("treat_"))
#>           treat_1         treat_2         treat_3 condition_a condition_b id
#> 1 masked_group_03 masked_group_01 masked_group_02           A           B  1
#> 2 masked_group_01 masked_group_02 masked_group_03           B           A  2
#> 3 masked_group_02 masked_group_03 masked_group_01           A           B  3
df |> mask_variables_rowwise(c("treat_1", "treat_2"))
#>           treat_1         treat_2   treat_3 condition_a condition_b id
#> 1 masked_group_02 masked_group_01   placebo           A           B  1
#> 2 masked_group_01 masked_group_03   control           B           A  2
#> 3 masked_group_03 masked_group_02 treatment           A           B  3
df |> mask_variables_rowwise(
  starts_with("treat_"),
  c("condition_a", "condition_b")
)
#>           treat_1         treat_2         treat_3     condition_a
#> 1 masked_group_02 masked_group_03 masked_group_01 masked_group_01
#> 2 masked_group_03 masked_group_01 masked_group_02 masked_group_02
#> 3 masked_group_01 masked_group_02 masked_group_03 masked_group_01
#>       condition_b id
#> 1 masked_group_02  1
#> 2 masked_group_01  2
#> 3 masked_group_02  3
```
