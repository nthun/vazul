# vazul

<!-- badges: start -->
[![R-CMD-check](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/nthun/vazul/graph/badge.svg)](https://app.codecov.io/gh/nthun/vazul)
<!-- badges: end -->

**vazul** is an R package that provides functions for data blinding, allowing researchers to scramble data while preserving its statistical properties. 

## Installation

You can install the development version of vazul from [GitHub](https://github.com/nthun/vazul) with:

``` r
# install.packages("devtools")
devtools::install_github("nthun/vazul")
```

## Main Functions

### `scramble_values()`

Scrambles the values within a vector without replacement, preserving the original values but changing their order.

``` r
library(vazul)

# Scramble numeric values
set.seed(123)
scramble_values(1:10)
#>  [1]  3 10  2  8  6  9  1  7  5  4

# Scramble character values  
scramble_values(letters[1:5])
#> [1] "b" "c" "a" "d" "e"

# Scramble logical values
scramble_values(c(TRUE, FALSE, TRUE, FALSE))
#> [1] FALSE  TRUE FALSE  TRUE
```

### `scramble_variables()`

Scrambles the values of specified variables (columns) within a data frame, with optional grouping.

``` r
# Create example data
df <- data.frame(
  x = 1:6, 
  y = letters[1:6], 
  group = c("A", "A", "A", "B", "B", "B")
)

# Scramble variables across entire dataset
scramble_variables(df, c("x", "y"))

# Scramble within groups using .groups parameter
scramble_variables(df, "y", .groups = "group")

# Or using dplyr grouping
library(dplyr)
df |> 
  group_by(group) |> 
  scramble_variables("x") |>
  ungroup()
  
# Can use multiple groups
marp |> 
    group_by(country, gender) |> 
    scramble_variables(c("rel_1", "rel_2")) |> 
    ungroup()
    
# Can use tidyselect helpers
marp |> 
    scramble_variables(starts_with("rel_")) |> 
    ungroup()
  
```

### `scramble_variables_rowwise()`

Scrambles the values of defined variable sets rowwise in a data frame. For each row, values within each variable set are shuffled while keeping the values within the same row.

``` r
# Create example data with multiple variable sets
df <- data.frame(
  day_1 = c(1, 4, 7),
  day_2 = c(2, 5, 8), 
  day_3 = c(3, 6, 9),
  score_a = c(10, 40, 70),
  score_b = c(20, 50, 80),
  id = 1:3
)

# Scramble a single set of variables rowwise
set.seed(123)
df |> scramble_variables_rowwise(c("day_1", "day_2", "day_3"))
#>   day_1 day_2 day_3 score_a score_b id
#> 1     3     1     2      10      20  1
#> 2     5     4     6      40      50  2
#> 3     8     9     7      70      80  3

# Scramble multiple sets of variables
df |> scramble_variables_rowwise(list(
  c("day_1", "day_2", "day_3"),
  c("score_a", "score_b")
))

# Using tidyselect helpers for single sets
library(dplyr)
df |> scramble_variables_rowwise(starts_with("day_"))

# Example with the 'williams' dataset
data(williams)

# Scramble sexual unrestrictedness items within each row
williams |> scramble_variables_rowwise(
  c("SexUnres_1", "SexUnres_2", "SexUnres_3")
)

# Scramble multiple construct sets
williams |> scramble_variables_rowwise(list(
  c("SexUnres_1", "SexUnres_2", "SexUnres_3"),
  c("Impuls_1", "Impuls_2_r", "Impul_3_r")
))
```

## Included Datasets

### MARP Dataset

The **Many Analysts Religion Project (MARP)** dataset contains data from 10,535 participants across 24 countries, exploring the relationship between religiosity and well-being.

``` r
data(marp)
dim(marp)
#> [1] 10535    46

# Explore countries in the dataset
length(unique(marp$country))
#> [1] 24

# Example: scramble religiosity variables by country
marp |>
  group_by(country) |>
  scramble_variables(c("rel_1", "rel_2", "rel_3")) |>
  ungroup()
```

**Variables include:**
- `rel_1` to `rel_9`: Religiosity measures
- `wb_*`: Well-being indicators (general, physical, psychological, social)
- `country`: Participant country
- `age`, `gender`, `ses`, `education`: Demographics
- `gdp`: Country-level GDP data

### Williams Dataset  

A study dataset with 112 participants examining the risk taking behavior behavior of high or low wealth individuals.

``` r
data(williams)
dim(williams)
#> [1] 112  25

table(williams$ecology)
#> Desperate   Hopeful 
#>        56        56

# Example: scramble perception measures within ecology conditions
williams |>
  group_by(ecology) |>
  scramble_variables(c("SexUnres_1", "Impuls_1")) |>
  ungroup()
```

**Key variables:**
- `ecology`: Experimental condition ("Desperate" vs "Hopeful")
- `SexUnres_*`: Sexual unresponsiveness measures  
- `Impuls_*`: Impulsivity measures
- `Opport_*`: Long-term planning opportunity measures
- `age`, `gender`: Participant demographics

## Explanation of the package name

Vazul was a historical figure (Hungarian price) in the 11. century. He was blinded by the king to become unfit for the throne. More info: https://en.wikipedia.org/wiki/Vazul

## Documentation

- Package documentation: `help(package = "vazul")`
- Function help: `?scramble_values`, `?scramble_variables`  
- Dataset documentation: `?marp`, `?williams`
- Package website: https://nthun.github.io/vazul/

## Authors

- **Tamás Nagy** - Package author and maintainer
- **Alexandra Sarafoglou** - Data contributor and author  
- **Márton Kovács** - Author

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
