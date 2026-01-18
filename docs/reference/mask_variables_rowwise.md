# Mask categorical labels across multiple columns rowwise in a data frame

For each row, independently mask labels within the selected columns. All
selected columns are combined into a single set and processed together.
To mask different variable groups separately, call the function multiple
times.

## Usage

``` r
mask_variables_rowwise(data, ..., prefix = "masked_group_")
```

## Arguments

- data:

  A data frame.

- ...:

  Columns to mask. All arguments are combined into a single set. Each
  can be:

  - Bare column names (e.g., `var1, var2`)

  - A tidyselect expression (e.g., `starts_with("treat_")`)

  - A character vector of column names (e.g., `c("var1", "var2")`)

- prefix:

  character string to use as prefix for masked labels. Default is
  "masked_group\_"

## Value

A data frame with labels masked rowwise within the selected columns.

## See also

[`mask_labels`](https://nthun.github.io/vazul/reference/mask_labels.md)
for masking a single vector,
[`mask_variables`](https://nthun.github.io/vazul/reference/mask_variables.md)
for masking multiple variables, and
[`mask_names`](https://nthun.github.io/vazul/reference/mask_names.md)
for masking variable names.

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

set.seed(1037)
# Mask one set of variables
library(dplyr)
df |> mask_variables_rowwise(starts_with("treat_"))
#>           treat_1         treat_2         treat_3 condition_a condition_b id
#> 1 masked_group_01 masked_group_02 masked_group_03           A           B  1
#> 2 masked_group_02 masked_group_03 masked_group_01           B           A  2
#> 3 masked_group_03 masked_group_01 masked_group_02           A           B  3

# Using character vectors
df |> mask_variables_rowwise(c("treat_1", "treat_2", "treat_3"))
#>           treat_1         treat_2         treat_3 condition_a condition_b id
#> 1 masked_group_01 masked_group_02 masked_group_03           A           B  1
#> 2 masked_group_02 masked_group_03 masked_group_01           B           A  2
#> 3 masked_group_03 masked_group_01 masked_group_02           A           B  3

# Mask multiple sets separately
df |>
  mask_variables_rowwise(starts_with("treat_")) |>
  mask_variables_rowwise(c("condition_a", "condition_b"))
#>           treat_1         treat_2         treat_3     condition_a
#> 1 masked_group_03 masked_group_01 masked_group_02 masked_group_01
#> 2 masked_group_01 masked_group_02 masked_group_03 masked_group_02
#> 3 masked_group_02 masked_group_03 masked_group_01 masked_group_01
#>       condition_b id
#> 1 masked_group_02  1
#> 2 masked_group_01  2
#> 3 masked_group_02  3

# Example with custom prefix
df |> mask_variables_rowwise(starts_with("treat_"), prefix = "group_")
#>    treat_1  treat_2  treat_3 condition_a condition_b id
#> 1 group_03 group_01 group_02           A           B  1
#> 2 group_01 group_02 group_03           B           A  2
#> 3 group_02 group_03 group_01           A           B  3
```
