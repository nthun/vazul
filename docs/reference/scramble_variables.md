# Scrambling the content of several variables in a data frame

Scramble the values of several selected variables in a data frame
simultaneously. Supports independent scrambling, joint scrambling, and
within-group scrambling.

## Usage

``` r
scramble_variables(data, ..., .groups = NULL, together = FALSE)
```

## Arguments

- data:

  a data frame

- ...:

  Columns to scramble. Each can be:

  - Bare column names (e.g., `var1, var2`)

  - A tidyselect expression (e.g., `starts_with("treat_")`)

  - A character vector of column names (e.g., `c("var1", "var2")`)

  - Multiple sets can be provided as separate arguments

- .groups:

  Optional grouping columns. Scrambling will be done within each group.
  Supports the same tidyselect syntax as column selection. Grouping
  columns must not overlap with the columns selected in `...`. If `data`
  is already a grouped `dplyr` data frame, existing grouping is ignored
  unless `.groups` is explicitly provided.

- together:

  logical. If `TRUE`, variables are scrambled together as a unit per
  row. Values across different variables are kept intact but assigned to
  different rows. If `FALSE` (default), each variable is scrambled
  independently.

## Value

A data frame with the specified columns scrambled. If grouping is
specified, scrambling is done within each group.

## See also

[`scramble_values`](https://nthun.github.io/vazul/reference/scramble_values.md)
for scrambling a single vector, and
[`scramble_variables_rowwise`](https://nthun.github.io/vazul/reference/scramble_variables_rowwise.md)
for rowwise scrambling.

## Examples

``` r
df <- data.frame(
  x = 1:6,
  y = letters[1:6],
  group = c("A", "A", "A", "B", "B", "B")
)

set.seed(123)
# Example without grouping. Variables scrambled across the entire data frame.
# Using bare names
df |> scramble_variables(x, y)
#>   x y group
#> 1 3 e     A
#> 2 6 d     A
#> 3 2 b     A
#> 4 4 f     B
#> 5 5 a     B
#> 6 1 c     B
# Or using character vector
df |> scramble_variables(c("x", "y"))
#>   x y group
#> 1 3 a     A
#> 2 5 f     A
#> 3 6 e     A
#> 4 4 c     B
#> 5 1 b     B
#> 6 2 d     B

# Example with together = TRUE. Variables scrambled together as a unit per row.
df |> scramble_variables(c("x", "y"), together = TRUE)
#>   x y group
#> 1 2 b     A
#> 2 1 a     A
#> 3 6 f     A
#> 4 3 c     B
#> 5 4 d     B
#> 6 5 e     B

# Example with grouping. Variable only scrambled within groups.
df |> scramble_variables("y", .groups = "group")
#> # A tibble: 6 × 3
#>       x y     group
#>   <int> <chr> <chr>
#> 1     1 a     A    
#> 2     2 c     A    
#> 3     3 b     A    
#> 4     4 d     B    
#> 5     5 e     B    
#> 6     6 f     B    

# Example combining grouping and together parameters
df |> scramble_variables(c("x", "y"), .groups = "group", together = TRUE)
#> # A tibble: 6 × 3
#>       x y     group
#>   <int> <chr> <chr>
#> 1     2 b     A    
#> 2     1 a     A    
#> 3     3 c     A    
#> 4     4 d     B    
#> 5     6 f     B    
#> 6     5 e     B    

# Example with tidyselect helpers
library(dplyr)
df |> scramble_variables(starts_with("x"))
#>   x y group
#> 1 3 a     A
#> 2 4 b     A
#> 3 1 c     A
#> 4 6 d     B
#> 5 5 e     B
#> 6 2 f     B
df |> scramble_variables(where(is.numeric), .groups = "group")
#> # A tibble: 6 × 3
#>       x y     group
#>   <int> <chr> <chr>
#> 1     2 a     A    
#> 2     1 b     A    
#> 3     3 c     A    
#> 4     4 d     B    
#> 5     6 e     B    
#> 6     5 f     B    

# Example with the 'williams' dataset
data(williams)
williams |> scramble_variables(c("ecology", "age"))
#> # A tibble: 112 × 25
#>    subject   SexUnres_1 SexUnres_2 SexUnres_3 SexUnres_4_r SexUnres_5_r Impuls_1
#>    <chr>          <dbl>      <dbl>      <dbl>        <dbl>        <dbl>    <dbl>
#>  1 A30MP4LX…          5          3          2            3            2        3
#>  2 A16X5FB3…          7          7          7            4            2        6
#>  3 A1E9D1OT…          2          4          6            3            3        3
#>  4 A16FPOYD…          5          4          5            3            3        4
#>  5 A11NOTVH…          5          5          6            3            3        5
#>  6 A3TDR6MX…          5          6          6            3            2        5
#>  7 A3OD4F0S…          5          6          5            3            4        5
#>  8 A123PBQD…          4          6          5            2            3        6
#>  9 A25NGIY5…          6          5          6            3            2        6
#> 10 A11WCFPJ…          4          4          4            1            1        1
#> # ℹ 102 more rows
#> # ℹ 18 more variables: Impuls_2_r <dbl>, Impul_3_r <dbl>, Opport_1 <dbl>,
#> #   Opport_2 <dbl>, Opport_3 <dbl>, Opport_4 <dbl>, Opport_5 <dbl>,
#> #   Opport_6_r <dbl>, InvEdu_1_r <dbl>, InvEdu_2_r <dbl>, InvChild_1 <dbl>,
#> #   InvChild_2_r <dbl>, age <dbl>, gender <dbl>, ecology <chr>,
#> #   duration_in_seconds <dbl>, attention_1 <dbl>, attention_2 <dbl>
williams |> scramble_variables(1:5)
#> # A tibble: 112 × 25
#>    subject   SexUnres_1 SexUnres_2 SexUnres_3 SexUnres_4_r SexUnres_5_r Impuls_1
#>    <chr>          <dbl>      <dbl>      <dbl>        <dbl>        <dbl>    <dbl>
#>  1 AABRVOZP…          3          5          4            2            2        3
#>  2 A16X5FB3…          3          7          4            3            2        6
#>  3 A36FQULQ…          6          4          4            2            3        3
#>  4 A2P4XT8Q…          6          4          5            6            3        4
#>  5 AGC7U237…          5          4          2            6            3        5
#>  6 A26N0W3Q…          4          4          2            4            2        5
#>  7 AQ5CSVCN…          5          4          4            3            4        5
#>  8 A3GFOV6P…          4          6          7            2            3        6
#>  9 A2TXK35P…          3          5          6            4            2        6
#> 10 A3J5PLM6…          5          5          4            3            1        1
#> # ℹ 102 more rows
#> # ℹ 18 more variables: Impuls_2_r <dbl>, Impul_3_r <dbl>, Opport_1 <dbl>,
#> #   Opport_2 <dbl>, Opport_3 <dbl>, Opport_4 <dbl>, Opport_5 <dbl>,
#> #   Opport_6_r <dbl>, InvEdu_1_r <dbl>, InvEdu_2_r <dbl>, InvChild_1 <dbl>,
#> #   InvChild_2_r <dbl>, age <dbl>, gender <dbl>, ecology <chr>,
#> #   duration_in_seconds <dbl>, attention_1 <dbl>, attention_2 <dbl>
williams |> scramble_variables(c("ecology", "age"), .groups = "gender")
#> # A tibble: 112 × 25
#>    subject   SexUnres_1 SexUnres_2 SexUnres_3 SexUnres_4_r SexUnres_5_r Impuls_1
#>    <chr>          <dbl>      <dbl>      <dbl>        <dbl>        <dbl>    <dbl>
#>  1 A30MP4LX…          5          3          2            3            2        3
#>  2 A16X5FB3…          7          7          7            4            2        6
#>  3 A1E9D1OT…          2          4          6            3            3        3
#>  4 A16FPOYD…          5          4          5            3            3        4
#>  5 A11NOTVH…          5          5          6            3            3        5
#>  6 A3TDR6MX…          5          6          6            3            2        5
#>  7 A3OD4F0S…          5          6          5            3            4        5
#>  8 A123PBQD…          4          6          5            2            3        6
#>  9 A25NGIY5…          6          5          6            3            2        6
#> 10 A11WCFPJ…          4          4          4            1            1        1
#> # ℹ 102 more rows
#> # ℹ 18 more variables: Impuls_2_r <dbl>, Impul_3_r <dbl>, Opport_1 <dbl>,
#> #   Opport_2 <dbl>, Opport_3 <dbl>, Opport_4 <dbl>, Opport_5 <dbl>,
#> #   Opport_6_r <dbl>, InvEdu_1_r <dbl>, InvEdu_2_r <dbl>, InvChild_1 <dbl>,
#> #   InvChild_2_r <dbl>, age <dbl>, gender <dbl>, ecology <chr>,
#> #   duration_in_seconds <dbl>, attention_1 <dbl>, attention_2 <dbl>
williams |> scramble_variables(c(1, 2), .groups = 3)
#> # A tibble: 112 × 25
#>    subject   SexUnres_1 SexUnres_2 SexUnres_3 SexUnres_4_r SexUnres_5_r Impuls_1
#>    <chr>          <dbl>      <dbl>      <dbl>        <dbl>        <dbl>    <dbl>
#>  1 A1LEB8GU…          6          3          2            3            2        3
#>  2 A1K6ZE1U…          7          7          7            4            2        6
#>  3 A2H95JVP…          4          4          6            3            3        3
#>  4 A2RBF3II…          5          4          5            3            3        4
#>  5 A9ZBEA4T…          5          5          6            3            3        5
#>  6 A5NE8TWS…          5          6          6            3            2        5
#>  7 A3TDR6MX…          5          6          5            3            4        5
#>  8 A2EELTS0…          5          6          5            2            3        6
#>  9 A2P4XT8Q…          5          5          6            3            2        6
#> 10 A2TXK35P…          3          4          4            1            1        1
#> # ℹ 102 more rows
#> # ℹ 18 more variables: Impuls_2_r <dbl>, Impul_3_r <dbl>, Opport_1 <dbl>,
#> #   Opport_2 <dbl>, Opport_3 <dbl>, Opport_4 <dbl>, Opport_5 <dbl>,
#> #   Opport_6_r <dbl>, InvEdu_1_r <dbl>, InvEdu_2_r <dbl>, InvChild_1 <dbl>,
#> #   InvChild_2_r <dbl>, age <dbl>, gender <dbl>, ecology <chr>,
#> #   duration_in_seconds <dbl>, attention_1 <dbl>, attention_2 <dbl>
williams |> scramble_variables(c("ecology", "age"), together = TRUE)
#> # A tibble: 112 × 25
#>    subject   SexUnres_1 SexUnres_2 SexUnres_3 SexUnres_4_r SexUnres_5_r Impuls_1
#>    <chr>          <dbl>      <dbl>      <dbl>        <dbl>        <dbl>    <dbl>
#>  1 A30MP4LX…          5          3          2            3            2        3
#>  2 A16X5FB3…          7          7          7            4            2        6
#>  3 A1E9D1OT…          2          4          6            3            3        3
#>  4 A16FPOYD…          5          4          5            3            3        4
#>  5 A11NOTVH…          5          5          6            3            3        5
#>  6 A3TDR6MX…          5          6          6            3            2        5
#>  7 A3OD4F0S…          5          6          5            3            4        5
#>  8 A123PBQD…          4          6          5            2            3        6
#>  9 A25NGIY5…          6          5          6            3            2        6
#> 10 A11WCFPJ…          4          4          4            1            1        1
#> # ℹ 102 more rows
#> # ℹ 18 more variables: Impuls_2_r <dbl>, Impul_3_r <dbl>, Opport_1 <dbl>,
#> #   Opport_2 <dbl>, Opport_3 <dbl>, Opport_4 <dbl>, Opport_5 <dbl>,
#> #   Opport_6_r <dbl>, InvEdu_1_r <dbl>, InvEdu_2_r <dbl>, InvChild_1 <dbl>,
#> #   InvChild_2_r <dbl>, age <dbl>, gender <dbl>, ecology <chr>,
#> #   duration_in_seconds <dbl>, attention_1 <dbl>, attention_2 <dbl>
williams |> scramble_variables(c("ecology", "age"), .groups = "gender", together = TRUE)
#> # A tibble: 112 × 25
#>    subject   SexUnres_1 SexUnres_2 SexUnres_3 SexUnres_4_r SexUnres_5_r Impuls_1
#>    <chr>          <dbl>      <dbl>      <dbl>        <dbl>        <dbl>    <dbl>
#>  1 A30MP4LX…          5          3          2            3            2        3
#>  2 A16X5FB3…          7          7          7            4            2        6
#>  3 A1E9D1OT…          2          4          6            3            3        3
#>  4 A16FPOYD…          5          4          5            3            3        4
#>  5 A11NOTVH…          5          5          6            3            3        5
#>  6 A3TDR6MX…          5          6          6            3            2        5
#>  7 A3OD4F0S…          5          6          5            3            4        5
#>  8 A123PBQD…          4          6          5            2            3        6
#>  9 A25NGIY5…          6          5          6            3            2        6
#> 10 A11WCFPJ…          4          4          4            1            1        1
#> # ℹ 102 more rows
#> # ℹ 18 more variables: Impuls_2_r <dbl>, Impul_3_r <dbl>, Opport_1 <dbl>,
#> #   Opport_2 <dbl>, Opport_3 <dbl>, Opport_4 <dbl>, Opport_5 <dbl>,
#> #   Opport_6_r <dbl>, InvEdu_1_r <dbl>, InvEdu_2_r <dbl>, InvChild_1 <dbl>,
#> #   InvChild_2_r <dbl>, age <dbl>, gender <dbl>, ecology <chr>,
#> #   duration_in_seconds <dbl>, attention_1 <dbl>, attention_2 <dbl>
```
