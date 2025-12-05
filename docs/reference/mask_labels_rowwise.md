# Mask categorical labels rowwise in a data frame

For each row, mask the categorical labels across the selected columns
using consistent masked labels. This function applies mask_labels() to
the unique values found across all selected columns to ensure
consistency.

## Usage

``` r
mask_labels_rowwise(data, cols, prefix = "masked_group_")
```

## Arguments

- data:

  a data frame

- cols:

  Columns to mask rowwise. Supports helpers like
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`where()`](https://tidyselect.r-lib.org/reference/where.html), etc.

- prefix:

  character string to use as prefix for masked labels. Default is
  "masked_group\_"

## Value

A data frame with labels masked rowwise across the selected columns.

## Examples

``` r
df <- data.frame(
  treat_1 = c("control", "treatment", "placebo"),
  treat_2 = c("treatment", "placebo", "control"),
  treat_3 = c("placebo", "control", "treatment"),
  id = 1:3
)

set.seed(123)
df |> mask_labels_rowwise(starts_with("treat_"))
#>           treat_1         treat_2         treat_3 id
#> 1 masked_group_03 masked_group_01 masked_group_02  1
#> 2 masked_group_01 masked_group_02 masked_group_03  2
#> 3 masked_group_02 masked_group_03 masked_group_01  3
df |> mask_labels_rowwise(c("treat_1", "treat_2"))
#>           treat_1         treat_2   treat_3 id
#> 1 masked_group_02 masked_group_01   placebo  1
#> 2 masked_group_01 masked_group_03   control  2
#> 3 masked_group_03 masked_group_02 treatment  3
```
