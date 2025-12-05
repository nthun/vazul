# Mask variable names with anonymous labels

Assigns new masked names to groups of variables in a data frame. Each
group of variables gets its own prefix for the masked names, such as
"variable_set_A_01", "variable_set_B_01", etc. The function ensures no
name collisions occur with existing column names.

## Usage

``` r
mask_names(data, ..., prefix = "variable_set_", set_id = NULL)
```

## Arguments

- data:

  a data frame

- ...:

  One or more variable sets. Each can be:

  - A tidyselect expression (e.g., `starts_with("treatment_")`)

  - A character vector of column names (e.g., `c("var1", "var2")`)

  - Multiple sets can be provided as separate arguments

- prefix:

  character string to use as base prefix for masked names. Default is
  "variable_set\_"

- set_id:

  character string to append to prefix for each variable set. If NULL
  (default), uses letters A, B, C, etc. for each set.

## Value

A data frame with the specified variables renamed to masked names.

## Examples

``` r
df <- data.frame(
  treat_1 = c(1, 2, 3),
  treat_2 = c(4, 5, 6),
  outcome_a = c(7, 8, 9),
  outcome_b = c(10, 11, 12),
  id = 1:3
)

# Mask two variable sets with default prefixes
mask_names(df,
  starts_with("treat_"),
  starts_with("outcome_")
)
#>   variable_set_A_01 variable_set_A_02 variable_set_B_02 variable_set_B_01 id
#> 1                 1                 4                 7                10  1
#> 2                 2                 5                 8                11  2
#> 3                 3                 6                 9                12  3

# Using character vectors
mask_names(df,
  c("treat_1", "treat_2"),
  c("outcome_a", "outcome_b")
)
#>   variable_set_A_01 variable_set_A_02 variable_set_B_02 variable_set_B_01 id
#> 1                 1                 4                 7                10  1
#> 2                 2                 5                 8                11  2
#> 3                 3                 6                 9                12  3

# Custom set_id
mask_names(df,
  starts_with("treat_"),
  starts_with("outcome_"),
  set_id = c("treatment", "outcome")
)
#>   variable_set_treatment_02 variable_set_treatment_01 variable_set_outcome_02
#> 1                         1                         4                       7
#> 2                         2                         5                       8
#> 3                         3                         6                       9
#>   variable_set_outcome_01 id
#> 1                      10  1
#> 2                      11  2
#> 3                      12  3

# Example with the 'williams' dataset
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

set.seed(42)

williams |>
mask_names(starts_with("SexUnres"),
           starts_with("Impul"),
           starts_with("Opport"),
           starts_with("InvEdu"),
           starts_with("InvChild"),
           ) |>
 glimpse()
#> Rows: 112
#> Columns: 25
#> $ subject             <chr> "A30MP4LXV4MIFD", "A16X5FB3HAFCKN", "A1E9D1OT9VJYD…
#> $ variable_set_A_01   <dbl> 5, 7, 2, 5, 5, 5, 5, 4, 6, 4, 5, 3, 5, 6, 3, 6, 2,…
#> $ variable_set_A_05   <dbl> 3, 7, 4, 4, 5, 6, 6, 6, 5, 4, 5, 2, 5, 3, 2, 3, 1,…
#> $ variable_set_A_04   <dbl> 2, 7, 6, 5, 6, 6, 5, 5, 6, 4, 7, 3, 5, 3, 3, 6, 3,…
#> $ variable_set_A_03   <dbl> 3, 4, 3, 3, 3, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 3, 4,…
#> $ variable_set_A_02   <dbl> 2, 2, 3, 3, 3, 2, 4, 3, 2, 1, 2, 2, 4, 6, 3, 1, 3,…
#> $ variable_set_B_02   <dbl> 3, 6, 3, 4, 5, 5, 5, 6, 6, 1, 6, 2, 4, 7, 5, 4, 5,…
#> $ variable_set_B_03   <dbl> 3, 2, 3, 4, 2, 3, 3, 2, 3, 1, 3, 2, 3, 6, 3, 3, 7,…
#> $ variable_set_B_01   <dbl> 2, 1, 3, 4, 4, 3, 2, 3, 2, 1, 4, 1, 4, 5, 3, 3, 5,…
#> $ variable_set_C_04   <dbl> 1, 5, 3, 4, 4, 4, 5, 5, 5, 1, 7, 3, 5, 7, 5, 2, 6,…
#> $ variable_set_C_01   <dbl> 2, 7, 5, 4, 6, 3, 5, 4, 4, 4, 6, 4, 6, 5, 2, 3, 4,…
#> $ variable_set_C_05   <dbl> 2, 7, 5, 4, 4, 4, 6, 5, 5, 1, 7, 3, 5, 5, 5, 3, 1,…
#> $ variable_set_C_02   <dbl> 3, 6, 3, 5, 4, 6, 5, 5, 4, 4, 6, 3, 6, 4, 4, 3, 3,…
#> $ variable_set_C_03   <dbl> 1, 6, 3, 4, 5, 4, 6, 6, 4, 1, 6, 3, 6, 5, 5, 4, 6,…
#> $ variable_set_C_06   <dbl> 3, 2, 3, 4, 3, 3, 2, 2, 3, 2, 1, 3, 1, 6, 4, 3, 4,…
#> $ variable_set_D_02   <dbl> 2, 3, 2, 3, 4, 3, 3, 3, 3, 2, 3, 4, 4, 4, 3, 1, 6,…
#> $ variable_set_D_01   <dbl> 3, 2, 3, 4, 3, 4, 2, 3, 4, 1, 2, 2, 3, 7, 4, 1, 3,…
#> $ variable_set_E_01   <dbl> 2, 5, 6, 5, 5, 5, 6, 5, 6, 1, 5, 2, 4, 4, 5, 2, 6,…
#> $ variable_set_E_02   <dbl> 3, 2, 3, 4, 3, 4, 2, 4, 3, 2, 4, 2, 3, 7, 3, 2, 6,…
#> $ age                 <dbl> 34, 30, 40, 35, 26, 33, 33, 30, 48, 33, 40, 39, 25…
#> $ gender              <dbl> 1, 1, 1, 2, 2, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1,…
#> $ ecology             <chr> "Hopeful", "Desperate", "Desperate", "Hopeful", "D…
#> $ duration_in_seconds <dbl> 164, 100, 47, 31, 40, 32, 98, 34, 32, 87, 71, 103,…
#> $ attention_1         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0,…
#> $ attention_2         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1,…

```
