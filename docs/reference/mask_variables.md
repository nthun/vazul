# Mask categorical variables with random labels in a data frame

Applies masked labels to multiple categorical variables in a data frame
using the
[`mask_labels()`](https://nthun.github.io/vazul/reference/mask_labels.md)
function. Each variable gets independent random masked labels by
default, or can optionally use the same masked labels across all
selected variables.

## Usage

``` r
mask_variables(data, ..., across_variables = FALSE)
```

## Arguments

- data:

  a data frame

- ...:

  Columns to mask. Each can be:

  - A tidyselect expression (e.g., `starts_with("treat_")`)

  - A character vector of column names (e.g., `c("var1", "var2")`)

  - Multiple sets can be provided as separate arguments

  Only character and factor columns will be processed.

- across_variables:

  logical. If `TRUE`, all selected variables will use the same set of
  masked labels. If `FALSE` (default), each variable gets its own
  independent set of masked labels using the column name as prefix.

## Value

A data frame with the specified categorical columns masked. Only
character and factor columns can be processed.

## Examples

``` r
# Create example data
df <- data.frame(
  treatment = c("control", "intervention", "control"),
  outcome = c("success", "failure", "success"),
  score = c(1, 2, 3)  # numeric, won't be masked
)

set.seed(123)
# Independent masking for each variable (default - uses column names as
# prefixes)
mask_variables(df, c("treatment", "outcome"))
#>            treatment          outcome score
#> 1 treatment_group_01 outcome_group_01     1
#> 2 treatment_group_02 outcome_group_02     2
#> 3 treatment_group_01 outcome_group_01     3

set.seed(456)
# Shared masking across variables
mask_variables(df, c("treatment", "outcome"), across_variables = TRUE)
#>         treatment         outcome score
#> 1 masked_group_01 masked_group_03     1
#> 2 masked_group_04 masked_group_02     2
#> 3 masked_group_01 masked_group_03     3

# Using tidyselect helpers
mask_variables(df, where(is.character))
#>            treatment          outcome score
#> 1 treatment_group_01 outcome_group_01     1
#> 2 treatment_group_02 outcome_group_02     2
#> 3 treatment_group_01 outcome_group_01     3

# Example with multiple categorical columns
df2 <- data.frame(
  group = c("A", "B", "A", "B"),
  condition = c("ctrl", "test", "ctrl", "test")
)
set.seed(123)
result <- mask_variables(df2, c("group", "condition"))
print(result)
#>            group          condition
#> 1 group_group_01 condition_group_01
#> 2 group_group_02 condition_group_02
#> 3 group_group_01 condition_group_01
#> 4 group_group_02 condition_group_02

# Example with williams dataset (multiple categorical columns)
data(williams)
set.seed(456)
williams_masked <- mask_variables(williams, c("subject", "ecology"))
head(williams_masked[c("subject", "ecology")])
#> # A tibble: 6 × 2
#>   subject           ecology         
#>   <chr>             <chr>           
#> 1 subject_group_109 ecology_group_02
#> 2 subject_group_101 ecology_group_01
#> 3 subject_group_035 ecology_group_01
#> 4 subject_group_038 ecology_group_02
#> 5 subject_group_085 ecology_group_01
#> 6 subject_group_027 ecology_group_01
```
