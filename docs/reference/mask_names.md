# Mask variable names with anonymous labels

Assigns new masked names to selected variables in a data frame. All
selected variables are combined into a single set and renamed with a
common prefix. To mask different variable groups with different
prefixes, call the function separately for each group.

## Usage

``` r
mask_names(data, ..., prefix, keep_suffixes = NULL)
```

## Arguments

- data:

  A data frame.

- ...:

  Columns to mask using tidyselect semantics. All arguments are combined
  into a single set. Each can be:

  - Bare column names (e.g., `var1, var2`)

  - A tidyselect expression (e.g., `starts_with("treatment_")`)

  - A character vector of column names (e.g., `c("var1", "var2")`)

- prefix:

  character string to use as prefix for masked names. This becomes the
  base prefix, with numeric suffixes appended (e.g.,
  `prefix = "treatment_"` produces "treatment_01", "treatment_02",
  etc.). The prefix is used as-is, so include a separator (e.g.,
  underscore) if desired.

- keep_suffixes:

  Optional character vector of suffixes to preserve in masked names.
  When provided, any selected column whose name ends with one of the
  supplied strings will have that suffix appended verbatim to its masked
  name (e.g. `treat_1_r` becomes `A_02_r` rather than `A_02`). Multiple
  suffixes may be supplied (e.g. `c("_r", "_z")`); when more than one
  suffix matches, the longest match takes precedence and a warning
  reports the affected column(s) and the overlapping suffixes. Duplicate
  entries are silently removed. Defaults to `NULL` (no suffixes
  preserved).

## Value

A data frame with the specified variables renamed to masked names. The
masked columns are sorted by their base masked name alphabetically
within their original positions; downstream code that accesses columns
by integer index rather than name may be affected. When `keep_suffixes`
is provided, columns whose original names end with a matching suffix
retain that suffix in their masked name.

## See also

[`mask_labels`](https://nthun.github.io/vazul/reference/mask_labels.md)
for masking values in a vector,
[`mask_variables`](https://nthun.github.io/vazul/reference/mask_variables.md)
for masking values in multiple variables.

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
mask_names(df, starts_with("treat_"), prefix = "A_")
#>   A_01 A_02 outcome_a outcome_b id
#> 1    1    4         7        10  1
#> 2    2    5         8        11  2
#> 3    3    6         9        12  3

# Using character vectors
mask_names(df, c("treat_1", "treat_2"), prefix = "A_")
#>   A_01 A_02 outcome_a outcome_b id
#> 1    1    4         7        10  1
#> 2    2    5         8        11  2
#> 3    3    6         9        12  3

# Preserve the _r suffix for reverse-scored items
set.seed(42)
df2 <- data.frame(
  treat_1   = c(1, 2, 3),
  treat_1_r = c(3, 2, 1),
  id = 1:3
)
mask_names(df2, starts_with("treat_"), prefix = "A_", keep_suffixes = "_r")
#>   A_01 A_02_r id
#> 1    1      3  1
#> 2    2      2  2
#> 3    3      1  3

# Mask multiple sets separately
# Note that the order of masking matters
# Try to mix up the order of prefixes
# for different sets to ensure proper masking.
df |>
  mask_names(starts_with("treat_"), prefix = "B_") |>
  mask_names(starts_with("outcome_"), prefix = "A_")
#>   B_01 B_02 A_01 A_02 id
#> 1    1    4   10    7  1
#> 2    2    5   11    8  2
#> 3    3    6   12    9  3

# Example with the 'williams' dataset
data(williams)
set.seed(42)

williams |>
  mask_names(starts_with("SexUnres"), prefix = "A_") |>
  mask_names(starts_with("Impul"), prefix = "B_") |>
  colnames()
#>  [1] "subject"             "A_01"                "A_02"               
#>  [4] "A_03"                "A_04"                "A_05"               
#>  [7] "B_01"                "B_02"                "B_03"               
#> [10] "Opport_1"            "Opport_2"            "Opport_3"           
#> [13] "Opport_4"            "Opport_5"            "Opport_6_r"         
#> [16] "InvEdu_1_r"          "InvEdu_2_r"          "InvChild_1"         
#> [19] "InvChild_2_r"        "age"                 "gender"             
#> [22] "ecology"             "duration_in_seconds" "attention_1"        
#> [25] "attention_2"        
```
