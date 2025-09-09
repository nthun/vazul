# vazul

<!-- badges: start -->
[![R-CMD-check](https://github.com/nthun/vazul/workflows/R-CMD-check/badge.svg)](https://github.com/nthun/vazul/actions)
<!-- badges: end -->

**vazul** is an R package that provides functions for data blinding, allowing researchers to scramble sensitive data while preserving its statistical properties. The package includes two main functions for data scrambling and two research datasets for demonstration and practice.

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

A stereotyping study dataset with 112 participants examining perceptions of high-wealth individuals in different ecological contexts.

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
- `SexUnres_*`: Sexual unrestrictedness measures  
- `Impuls_*`: Impulsivity measures
- `Opport_*`: Long-term planning opportunity measures
- `age`, `gender`: Participant demographics

## Use Cases

The vazul package is particularly useful for:

- **Data sharing**: Scramble sensitive variables before sharing datasets
- **Simulation studies**: Create variations of existing data while preserving structure
- **Teaching**: Demonstrate statistical concepts with modified real data
- **Robustness testing**: Test analysis pipelines with scrambled data

## Documentation

- Package documentation: `help(package = "vazul")`
- Function help: `?scramble_values`, `?scramble_variables`  
- Dataset documentation: `?marp`, `?williams`
- Package website: https://nthun.github.io/vazul/

## Authors

- **Tamás Nagy** - Package author and maintainer
- **Alexandra Sarafoglou** - Data contributor  
- **Márton Kovács** - Author

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.