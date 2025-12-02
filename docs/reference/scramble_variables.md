# Scrambling the content of several variables in a data frame

Scramble the values of several variables in a data frame.

## Usage

``` r
scramble_variables(data, cols, .groups = NULL, together = FALSE)
```

## Arguments

- data:

  a data frame

- cols:

  Columns to scramble. Accepts column names, positions, or tidyselect
  helpers like
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`where()`](https://tidyselect.r-lib.org/reference/where.html), etc.

- .groups:

  Optional grouping columns. Scrambling will be done within each group.
  Supports same tidyselect syntax as `cols`.

- together:

  logical. If TRUE, variables are scrambled together as a unit per row.
  Values across different variables are kept intact but assigned to
  different rows. If FALSE (default), each variable is scrambled
  independently.

## Value

A data frame with the specified columns scrambled. If grouping is
specified, scrambling is done within each group.

## Examples

``` r
df <- data.frame(x = 1:6, y = letters[1:6], group = c("A", "A", "A", "B", "B", "B"))

set.seed(123)
# Example without grouping. Variables scrambled across the entire data frame.
df |> scramble_variables(c("x", "y"))
#>   x y group
#> 1 3 e     A
#> 2 6 d     A
#> 3 2 b     A
#> 4 4 f     B
#> 5 5 a     B
#> 6 1 c     B

# Example with together = TRUE. Variables scrambled together as a unit per row.
df |> scramble_variables(c("x", "y"), together = TRUE)
#>   x y group
#> 1 3 c     A
#> 2 5 e     A
#> 3 6 f     A
#> 4 4 d     B
#> 5 1 a     B
#> 6 2 b     B

# Example with grouping. Variable only scrambled within groups.
df |> scramble_variables("y", .groups = "group")
#> # A tibble: 6 × 3
#>       x y     group
#>   <int> <chr> <chr>
#> 1     1 a     A    
#> 2     2 c     A    
#> 3     3 b     A    
#> 4     4 f     B    
#> 5     5 e     B    
#> 6     6 d     B    

# Example combining grouping and together parameters
df |> scramble_variables(c("x", "y"), .groups = "group", together = TRUE)
#> # A tibble: 6 × 3
#>       x y     group
#>   <int> <chr> <chr>
#> 1     3 c     A    
#> 2     2 b     A    
#> 3     1 a     A    
#> 4     5 e     B    
#> 5     4 d     B    
#> 6     6 f     B    

# Example with tidyselect helpers
library(dplyr)
df |> scramble_variables(starts_with("x"))
#>   x y group
#> 1 6 a     A
#> 2 1 b     A
#> 3 3 c     A
#> 4 4 d     B
#> 5 5 e     B
#> 6 2 f     B
df |> scramble_variables(where(is.numeric), .groups = "group")
#> # A tibble: 6 × 3
#>       x y     group
#>   <int> <chr> <chr>
#> 1     3 a     A    
#> 2     2 b     A    
#> 3     1 c     A    
#> 4     4 d     B    
#> 5     6 e     B    
#> 6     5 f     B    

# Example with the 'williams' dataset
if (requireNamespace("dplyr", quietly = TRUE)) {
  data(williams, package = "vazul")
  williams |> scramble_variables(c("ecology", "age"))
  williams |> scramble_variables(1:5)
  williams |> scramble_variables(c("ecology", "age"), .groups = "gender")
  williams |> scramble_variables(c(1, 2), .groups = c(3))
  williams |> scramble_variables(c("ecology", "age"), together = TRUE)
  williams |> scramble_variables(c("ecology", "age"), .groups = "gender", together = TRUE)
}
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
