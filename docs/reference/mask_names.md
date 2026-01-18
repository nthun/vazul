# Mask variable names with anonymous labels

Assigns new masked names to selected variables in a data frame. All
selected variables are combined into a single set and renamed with a
common prefix. To mask different variable groups with different
prefixes, call the function separately for each group.

## Usage

``` r
mask_names(data, ..., prefix)
```

## Arguments

- data:

  A data frame.

- ...:

  Columns to mask. All arguments are combined into a single set. Each
  can be:

  - Bare column names (e.g., `var1, var2`)

  - A tidyselect expression (e.g., `starts_with("treatment_")`)

  - A character vector of column names (e.g., `c("var1", "var2")`)

- prefix:

  character string to use as prefix for masked names. This becomes the
  base prefix, with numeric suffixes appended (e.g.,
  `prefix = "treatment_"` produces "treatment_01", "treatment_02",
  etc.). The prefix is used as-is, so include a separator (e.g.,
  underscore) if desired.

## Value

A data frame with the specified variables renamed to masked names.

## See also

[`mask_labels`](https://nthun.github.io/vazul/reference/mask_labels.md)
for masking values in a vector,
[`mask_variables`](https://nthun.github.io/vazul/reference/mask_variables.md)
for masking values in multiple variables, and
[`mask_variables_rowwise`](https://nthun.github.io/vazul/reference/mask_variables_rowwise.md)
for rowwise value masking.

## Examples

``` r
df <- data.frame(
  treat_1 = c(1, 2, 3),
  treat_2 = c(4, 5, 6),
  outcome_a = c(7, 8, 9),
  outcome_b = c(10, 11, 12),
  id = 1:3
)

# Mask one set of variables
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
mask_names(df, starts_with("treat_"), prefix = "A_")
#>   A_01 A_02 outcome_a outcome_b id
#> 1    1    4         7        10  1
#> 2    2    5         8        11  2
#> 3    3    6         9        12  3

# Using character vectors
mask_names(df, c("treat_1", "treat_2"), prefix = "A_")
#>   A_02 A_01 outcome_a outcome_b id
#> 1    1    4         7        10  1
#> 2    2    5         8        11  2
#> 3    3    6         9        12  3

# Mask multiple sets separately
# Note that the order of masking matters
# Try to mix up the order of prefixes
# for different sets to ensure proper masking.
df |>
  mask_names(starts_with("treat_"), prefix = "B_") |>
  mask_names(starts_with("outcome_"), prefix = "A_")
#>   B_01 B_02 A_02 A_01 id
#> 1    1    4    7   10  1
#> 2    2    5    8   11  2
#> 3    3    6    9   12  3

# Example with the 'williams' dataset
data(williams)
set.seed(42)

williams |>
  mask_names(starts_with("SexUnres"), prefix = "A_") |>
  mask_names(starts_with("Impul"), prefix = "B_") |>
  colnames()
#>  [1] "subject"             "A_01"                "A_05"               
#>  [4] "A_04"                "A_03"                "A_02"               
#>  [7] "B_02"                "B_03"                "B_01"               
#> [10] "Opport_1"            "Opport_2"            "Opport_3"           
#> [13] "Opport_4"            "Opport_5"            "Opport_6_r"         
#> [16] "InvEdu_1_r"          "InvEdu_2_r"          "InvChild_1"         
#> [19] "InvChild_2_r"        "age"                 "gender"             
#> [22] "ecology"             "duration_in_seconds" "attention_1"        
#> [25] "attention_2"        
```
