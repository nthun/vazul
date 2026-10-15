# Masking variable names

In certain studies, variable names should be masked to prevent
researcher bias. Examples can include exploratory factor analysis,
network analysis, etc. The `vazul` package provides means to mask
variable names in a dataset, ensuring that analyses can be conducted
without preconceived notions about the variables.

``` r
library(vazul)
library(dplyr)
library(stats)
```

In this example, we will use the `williams` dataset from the
[vazul](https://nthun.github.io/vazul/) package.

``` r
data("williams", package = "vazul")

head(williams)
#> # A tibble: 6 × 25
#>   subject    SexUnres_1 SexUnres_2 SexUnres_3 SexUnres_4_r SexUnres_5_r Impuls_1
#>   <chr>           <dbl>      <dbl>      <dbl>        <dbl>        <dbl>    <dbl>
#> 1 A30MP4LXV…          5          3          2            3            2        3
#> 2 A16X5FB3H…          7          7          7            4            2        6
#> 3 A1E9D1OT9…          2          4          6            3            3        3
#> 4 A16FPOYD7…          5          4          5            3            3        4
#> 5 A11NOTVHW…          5          5          6            3            3        5
#> 6 A3TDR6MXS…          5          6          6            3            2        5
#> # ℹ 18 more variables: Impuls_2_r <dbl>, Impul_3_r <dbl>, Opport_1 <dbl>,
#> #   Opport_2 <dbl>, Opport_3 <dbl>, Opport_4 <dbl>, Opport_5 <dbl>,
#> #   Opport_6_r <dbl>, InvEdu_1_r <dbl>, InvEdu_2_r <dbl>, InvChild_1 <dbl>,
#> #   InvChild_2_r <dbl>, age <dbl>, gender <dbl>, ecology <chr>,
#> #   duration_in_seconds <dbl>, attention_1 <dbl>, attention_2 <dbl>
glimpse(williams)
#> Rows: 112
#> Columns: 25
#> $ subject             <chr> "A30MP4LXV4MIFD", "A16X5FB3HAFCKN", "A1E9D1OT9VJYD…
#> $ SexUnres_1          <dbl> 5, 7, 2, 5, 5, 5, 5, 4, 6, 4, 5, 3, 5, 6, 3, 6, 2,…
#> $ SexUnres_2          <dbl> 3, 7, 4, 4, 5, 6, 6, 6, 5, 4, 5, 2, 5, 3, 2, 3, 1,…
#> $ SexUnres_3          <dbl> 2, 7, 6, 5, 6, 6, 5, 5, 6, 4, 7, 3, 5, 3, 3, 6, 3,…
#> $ SexUnres_4_r        <dbl> 3, 4, 3, 3, 3, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 3, 4,…
#> $ SexUnres_5_r        <dbl> 2, 2, 3, 3, 3, 2, 4, 3, 2, 1, 2, 2, 4, 6, 3, 1, 3,…
#> $ Impuls_1            <dbl> 3, 6, 3, 4, 5, 5, 5, 6, 6, 1, 6, 2, 4, 7, 5, 4, 5,…
#> $ Impuls_2_r          <dbl> 3, 2, 3, 4, 2, 3, 3, 2, 3, 1, 3, 2, 3, 6, 3, 3, 7,…
#> $ Impul_3_r           <dbl> 2, 1, 3, 4, 4, 3, 2, 3, 2, 1, 4, 1, 4, 5, 3, 3, 5,…
#> $ Opport_1            <dbl> 1, 5, 3, 4, 4, 4, 5, 5, 5, 1, 7, 3, 5, 7, 5, 2, 6,…
#> $ Opport_2            <dbl> 2, 7, 5, 4, 6, 3, 5, 4, 4, 4, 6, 4, 6, 5, 2, 3, 4,…
#> $ Opport_3            <dbl> 2, 7, 5, 4, 4, 4, 6, 5, 5, 1, 7, 3, 5, 5, 5, 3, 1,…
#> $ Opport_4            <dbl> 3, 6, 3, 5, 4, 6, 5, 5, 4, 4, 6, 3, 6, 4, 4, 3, 3,…
#> $ Opport_5            <dbl> 1, 6, 3, 4, 5, 4, 6, 6, 4, 1, 6, 3, 6, 5, 5, 4, 6,…
#> $ Opport_6_r          <dbl> 3, 2, 3, 4, 3, 3, 2, 2, 3, 2, 1, 3, 1, 6, 4, 3, 4,…
#> $ InvEdu_1_r          <dbl> 2, 3, 2, 3, 4, 3, 3, 3, 3, 2, 3, 4, 4, 4, 3, 1, 6,…
#> $ InvEdu_2_r          <dbl> 3, 2, 3, 4, 3, 4, 2, 3, 4, 1, 2, 2, 3, 7, 4, 1, 3,…
#> $ InvChild_1          <dbl> 2, 5, 6, 5, 5, 5, 6, 5, 6, 1, 5, 2, 4, 4, 5, 2, 6,…
#> $ InvChild_2_r        <dbl> 3, 2, 3, 4, 3, 4, 2, 4, 3, 2, 4, 2, 3, 7, 3, 2, 6,…
#> $ age                 <dbl> 34, 30, 40, 35, 26, 33, 33, 30, 48, 33, 40, 39, 25…
#> $ gender              <dbl> 1, 1, 1, 2, 2, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1,…
#> $ ecology             <chr> "Hopeful", "Desperate", "Desperate", "Hopeful", "D…
#> $ duration_in_seconds <dbl> 164, 100, 47, 31, 40, 32, 98, 34, 32, 87, 71, 103,…
#> $ attention_1         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0,…
#> $ attention_2         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1,…
```

We will apply masking to the variables related to life history strategy,
which are prefixed with `SexUnres`, `Impuls`, `Opport`, `InvEdu`, and
`InvChild`. We’ll mask each variable group separately with randomized
letter prefixes (e.g., `C_01`, `A_01`, `E_01`, etc.). This way,
variables within the same original scale keep a common prefix for the
analysis, but analysts won’t know which prefix corresponds to which
original scale due to the randomization.

``` r
set.seed(84)

# Sample 5 random letters for the 5 variable groups
random_prefixes <- paste0(sample(LETTERS, 5), "_")

masked_williams <-
    williams |> 
    mask_names(starts_with("SexUnres"), prefix = random_prefixes[1]) |>
    mask_names(starts_with("Impul"), prefix = random_prefixes[2]) |>
    mask_names(starts_with("Opport"), prefix = random_prefixes[3]) |>
    mask_names(starts_with("InvEdu"), prefix = random_prefixes[4]) |>
    mask_names(starts_with("InvChild"), prefix = random_prefixes[5])

# Show the randomized prefixes used (but not which corresponds to which)
sort(unique(sub("_.*", "_", grep("^[A-Z]_", names(masked_williams), value = TRUE))))
#> [1] "L_" "P_" "S_" "V_" "Y_"
```

We can now perform an exploratory factor analysis (EFA) on the masked
variables. Since the variable names are masked with randomized prefixes,
we won’t know which original variables correspond to which factor, thus
preventing bias in interpreting the results.

``` r

set.seed(123)
efa_blind <-
    masked_williams |> 
    select(matches("^[A-Z]_")) |>
    factanal(factors = 5, rotation = "varimax")
    
# Get the loadings of the EFA on the masked data
efa_blind |> 
    loadings() |> 
    print(cutoff = 0.3, sort = TRUE)
#> 
#> Loadings:
#>      Factor1 Factor2 Factor3 Factor4 Factor5
#> S_01  0.749           0.331                 
#> S_03  0.701                                 
#> S_05  0.666           0.472                 
#> V_01  0.762                                 
#> P_01  0.870                                 
#> P_03  0.893                                 
#> P_04  0.758                                 
#> P_05  0.853                                 
#> P_06  0.834                                 
#> L_01  0.766                                 
#> S_02          0.567           0.425         
#> S_04          0.735   0.335                 
#> V_02          0.867                         
#> V_03          0.746                         
#> P_02          0.715                         
#> Y_01          0.839                         
#> Y_02          0.699                         
#> L_02          0.852                         
#> 
#>                Factor1 Factor2 Factor3 Factor4 Factor5
#> SS loadings      6.398   4.815   0.717   0.345   0.284
#> Proportion Var   0.355   0.267   0.040   0.019   0.016
#> Cumulative Var   0.355   0.623   0.663   0.682   0.698
```

The loading table shows that factors are not necessarily loading to
their original category. By using masking, researchers may be able to
make decisions without being biased on the variable names.

Applying the same analysis on the original dataset reveals the names of
the variables. Please note that the loadings may differ slightly due to
the randomness in the factor analysis process.

## Preserving a fixed suffix with `keep_suffixes`

Some of the `williams` columns end in `_r`, marking items that are
reverse-scored (e.g. `SexUnres_4_r`, `Impuls_2_r`). That suffix is not
itself sensitive information to hide — it is a fixed analysis-relevant
tag that a researcher needs to see in order to reverse-code the item
correctly before scoring, even while the rest of the name stays masked.
By default,
[`mask_names()`](https://nthun.github.io/vazul/reference/mask_names.md)
masks the suffix away along with everything else, so `SexUnres_4_r`
becomes an opaque `C_02` with no indication that it needs to be
reverse-coded.

The `keep_suffixes` argument preserves one or more literal suffixes
verbatim in the masked name, so the reverse-coding information survives
masking:

``` r
set.seed(84)
masked_with_suffix <-
    williams |>
    mask_names(starts_with("SexUnres"), prefix = "C_", keep_suffixes = "_r")

masked_with_suffix |>
    select(matches("^C_")) |>
    names()
#> [1] "C_01_r" "C_02"   "C_03"   "C_04"   "C_05_r"
```

Notice that the two reverse-scored columns keep their `_r` suffix
(e.g. `C_01_r`) while the rest of the name is masked as usual, and that
masked columns are also sorted alphabetically by their masked name. If
more than one supplied suffix could match the same column, the longest
match is kept and
[`mask_names()`](https://nthun.github.io/vazul/reference/mask_names.md)
issues a warning naming the affected column(s).

`keep_suffixes` covers the common case of a single, fixed suffix that
should always be preserved literally (not remapped to a randomized
label). When *both* the prefix and the suffix carry meaning that must be
masked - but consistently, so the same value always maps to the same
masked label - the long-format approach shown next is the right tool
instead.

## Masking names with meaningful prefixes *and* suffixes

Sometimes both the prefix and the suffix of a set of column names carry
meaning that should stay consistent across columns. For example,
`exp_pre`, `exp_post`, `ctl_pre`, and `ctl_post` encode both an
experimental condition (`exp`/`ctl`) and a measurement occasion
(`pre`/`post`).

Masking these names directly with repeated
[`mask_names()`](https://nthun.github.io/vazul/reference/mask_names.md)
calls, as above, does not preserve that consistency: each call assigns
new labels independently, so there is no guarantee that, say, `pre` is
masked to the same label in the `exp_` columns as in the `ctl_` columns.
In this situation we recommend reshaping the data to long format first,
so that the condition and occasion information become *values* in their
own columns rather than fragments of the column names.
[`mask_variables()`](https://nthun.github.io/vazul/reference/mask_variables.md)
can then mask those columns directly, which guarantees that a given
value (e.g. `"exp"` or `"pre"`) is always mapped to the same masked
label everywhere it occurs. If a wide format is required for the
confirmatory analysis, the masked data can be reshaped back afterward.

``` r
library(tidyr)

wide_demo <- data.frame(
  id = 1:4,
  exp_pre = c(10, 12, 9, 11),
  exp_post = c(15, 14, 13, 16),
  ctl_pre = c(8, 9, 10, 7),
  ctl_post = c(9, 10, 11, 8)
)
wide_demo
#>   id exp_pre exp_post ctl_pre ctl_post
#> 1  1      10       15       8        9
#> 2  2      12       14       9       10
#> 3  3       9       13      10       11
#> 4  4      11       16       7        8
```

``` r
# 1) Reshape to long format, splitting the column names into
#    a `condition` and a `time` variable
long_demo <- wide_demo |>
  pivot_longer(
    cols = -id,
    names_to = c("condition", "time"),
    names_sep = "_"
  )
long_demo
#> # A tibble: 16 × 4
#>       id condition time  value
#>    <int> <chr>     <chr> <dbl>
#>  1     1 exp       pre      10
#>  2     1 exp       post     15
#>  3     1 ctl       pre       8
#>  4     1 ctl       post      9
#>  5     2 exp       pre      12
#>  6     2 exp       post     14
#>  7     2 ctl       pre       9
#>  8     2 ctl       post     10
#>  9     3 exp       pre       9
#> 10     3 exp       post     13
#> 11     3 ctl       pre      10
#> 12     3 ctl       post     11
#> 13     4 exp       pre      11
#> 14     4 exp       post     16
#> 15     4 ctl       pre       7
#> 16     4 ctl       post      8
```

``` r
# 2) Mask the condition and time variables. Each variable keeps its own,
#    internally consistent mapping (every "exp" becomes the same masked
#    label, every "pre" becomes the same masked label, and so on).
set.seed(2024)
long_masked <- long_demo |>
  mask_variables(condition, time)

long_masked |>
  count(condition, time)
#> # A tibble: 4 × 3
#>   condition          time              n
#>   <chr>              <chr>         <int>
#> 1 condition_group_01 time_group_01     4
#> 2 condition_group_01 time_group_02     4
#> 3 condition_group_02 time_group_01     4
#> 4 condition_group_02 time_group_02     4
```

``` r
# 3) Reshape back to wide format if a wide layout is needed for analysis
long_masked |>
  unite(masked_name, condition, time) |>
  pivot_wider(names_from = masked_name, values_from = value)
#> # A tibble: 4 × 5
#>      id condition_group_02_time_…¹ condition_group_02_t…² condition_group_01_t…³
#>   <int>                      <dbl>                  <dbl>                  <dbl>
#> 1     1                         10                     15                      8
#> 2     2                         12                     14                      9
#> 3     3                          9                     13                     10
#> 4     4                         11                     16                      7
#> # ℹ abbreviated names: ¹​condition_group_02_time_group_01,
#> #   ²​condition_group_02_time_group_02, ³​condition_group_01_time_group_01
#> # ℹ 1 more variable: condition_group_01_time_group_02 <dbl>
```

Because masking is applied to `condition` and `time` as data values
rather than as fragments of the column names, the masked labels for
`"exp"`/`"ctl"` and `"pre"`/`"post"` are consistent across every column,
both before and after reshaping. We recommend this long-format workflow
whenever meaningful prefixes and suffixes both need to be masked;
[`mask_names()`](https://nthun.github.io/vazul/reference/mask_names.md)
remains the simpler choice when only a single, non-overlapping piece of
the name needs masking, and `mask_names(..., keep_suffixes = ...)`
covers the case of a fixed literal suffix that should be preserved as-is
rather than masked (see above).

``` r
set.seed(123)
efa_orig <-
    williams |> 
    select(SexUnres_1:InvChild_2_r) |>
    factanal(factors = 5, rotation = "varimax")

# Get the loadings of the EFA on the original data

efa_orig |> 
    loadings() |> 
    print(cutoff = 0.3, sort = TRUE)
#> 
#> Loadings:
#>              Factor1 Factor2 Factor3 Factor4 Factor5
#> SexUnres_1    0.666           0.472                 
#> SexUnres_2    0.701                                 
#> SexUnres_3    0.749           0.331                 
#> Impuls_1      0.762                                 
#> Opport_1      0.893                                 
#> Opport_2      0.834                                 
#> Opport_3      0.870                                 
#> Opport_4      0.758                                 
#> Opport_5      0.853                                 
#> InvChild_1    0.766                                 
#> SexUnres_4_r          0.735   0.335                 
#> SexUnres_5_r          0.567           0.425         
#> Impuls_2_r            0.867                         
#> Impul_3_r             0.746                         
#> Opport_6_r            0.715                         
#> InvEdu_1_r            0.699                         
#> InvEdu_2_r            0.839                         
#> InvChild_2_r          0.852                         
#> 
#>                Factor1 Factor2 Factor3 Factor4 Factor5
#> SS loadings      6.398   4.815   0.717   0.345   0.284
#> Proportion Var   0.355   0.267   0.040   0.019   0.016
#> Cumulative Var   0.355   0.623   0.663   0.682   0.698
```
