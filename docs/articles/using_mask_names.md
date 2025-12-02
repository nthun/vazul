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
`InvChild`. The
[`mask_names()`](https://nthun.github.io/vazul/reference/mask_names.md)
function will rename these variables to generic names like
`variable_set_A_01`, `variable_set_A_02`, etc., where “A” indicates the
first set of variables being masked. This way, we can specify variables
that should load to the same component without knowing their actual
names. Note that the order of new names is randomized, so we don’t
exactly know which original variable corresponds to which masked name..
The
[`mask_names()`](https://nthun.github.io/vazul/reference/mask_names.md)
function will rename these variables to generic names like
`variable_set_A_01`, `variable_set_A_02`, etc., where “A” indicates the
first set of variables being masked. This way, we can specify variables
that should load to the same component without knowing their actual
names. Note that the order of new names is randomized, so we don’t
exactly know which original variable corresponds to which masked name.

``` r
set.seed(84)

masked_williams <-
    williams |> 
    mask_names(starts_with("SexUnres"),
               starts_with("Impul"),
               starts_with("Opport"),
               starts_with("InvEdu"),
               starts_with("InvChild"))
```

We can now perform an exploratory factor analysis (EFA) on the masked
variables. Since the variable names are masked, we won’t know which
original variables correspond to which factor, thus preventing bias in
interpreting the results.

``` r

set.seed(123)
efa_blind <-
    masked_williams |> 
    select(starts_with("variable_set_")) |>
    factanal(factors = 5, rotation = "varimax")
    
# Get the loadings of the EFA on the masked data
efa_blind |> 
    loadings() |> 
    print(cutoff = 0.3, sort = TRUE)
#> 
#> Loadings:
#>                   Factor1 Factor2 Factor3 Factor4 Factor5
#> variable_set_A_03  0.666           0.472                 
#> variable_set_A_04  0.701                                 
#> variable_set_A_02  0.749           0.331                 
#> variable_set_B_02  0.762                                 
#> variable_set_C_05  0.893                                 
#> variable_set_C_06  0.834                                 
#> variable_set_C_02  0.870                                 
#> variable_set_C_01  0.758                                 
#> variable_set_C_04  0.853                                 
#> variable_set_E_01  0.766                                 
#> variable_set_A_01          0.735   0.335                 
#> variable_set_A_05          0.567           0.425         
#> variable_set_B_03          0.867                         
#> variable_set_B_01          0.746                         
#> variable_set_C_03          0.715                         
#> variable_set_D_01          0.699                         
#> variable_set_D_02          0.839                         
#> variable_set_E_02          0.852                         
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
