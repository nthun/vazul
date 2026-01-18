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
#>        Factor1 Factor2 Factor3 Factor4 Factor5
#> S_A_05  0.666           0.472                 
#> S_A_03  0.701                                 
#> S_A_01  0.749           0.331                 
#> V_A_01  0.762                                 
#> P_A_03  0.893                                 
#> P_A_06  0.834                                 
#> P_A_01  0.870                                 
#> P_A_04  0.758                                 
#> P_A_05  0.853                                 
#> L_A_01  0.766                                 
#> S_A_04          0.735   0.335                 
#> S_A_02          0.567           0.425         
#> V_A_02          0.867                         
#> V_A_03          0.746                         
#> P_A_02          0.715                         
#> Y_A_02          0.699                         
#> Y_A_01          0.839                         
#> L_A_02          0.852                         
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
