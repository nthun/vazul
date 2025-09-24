# vazul

<!-- badges: start -->
[![R-CMD-check](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/nthun/vazul/graph/badge.svg)](https://app.codecov.io/gh/nthun/vazul)
<!-- badges: end -->

**vazul** provides functions for data blinding in research contexts. Data blinding helps researchers analyze data without knowing the specific group assignments or values, reducing potential bias in analysis and interpretation.

## Data Blinding: Masking vs Scrambling

This package offers two main approaches to data blinding:

- **Masking** replaces categorical labels with anonymous identifiers (e.g., "Control" → "Group_01")
- **Scrambling** shuffles existing values within specified constraints while preserving the original data distribution 

## Installation

``` r
# install.packages("devtools")
devtools::install_github("nthun/vazul")
```

## Functions

### Masking Functions

#### `mask_labels()` - Mask Categorical Labels

Replaces categorical values with anonymous labels to blind group assignments:

``` r
library(vazul)

# Mask treatment conditions
set.seed(123) 
treatment <- c("control", "treatment", "control", "treatment")
mask_labels(treatment)
#> [1] "masked_group_01" "masked_group_02" "masked_group_01" "masked_group_02"

# Custom prefix
mask_labels(c("High", "Medium", "Low"), prefix = "condition_")
#> [1] "condition_02" "condition_01" "condition_03"
```

### Scrambling Functions

#### `scramble_values()` - Scramble Vector Values

Shuffles values within a vector:

``` r
# Scramble numeric values
set.seed(123)
scramble_values(1:10)
#>  [1]  3 10  2  8  6  9  1  7  5  4

# Scramble character values  
scramble_values(letters[1:5])
#> [1] "b" "c" "a" "d" "e"
```

#### `scramble_variables()` - Scramble Column Values

Scrambles values within specified columns, optionally by groups:

``` r
df <- data.frame(
  score = 1:6, 
  group = c("A", "A", "A", "B", "B", "B")
)

# Scramble across entire column
scramble_variables(df, "score")

# Scramble within groups
scramble_variables(df, "score", .groups = "group")

# Using dplyr grouping
library(dplyr)
df |> group_by(group) |> scramble_variables("score") |> ungroup()
```

#### `scramble_values_rowwise()` - Scramble Values Within Rows

Shuffles values across columns within each row:

``` r
df <- data.frame(
  day1 = c(1, 4, 7),
  day2 = c(2, 5, 8), 
  day3 = c(3, 6, 9)
)

set.seed(123)
scramble_values_rowwise(df, c("day1", "day2", "day3"))
#>   day1 day2 day3
#> 1    3    1    2
#> 2    5    4    6
#> 3    8    9    7
```

#### `scramble_variables_rowwise()` - Scramble Variable Sets Within Rows

Scrambles multiple sets of variables within each row:

``` r
df <- data.frame(
  item1 = c(1, 4), item2 = c(2, 5), item3 = c(3, 6),
  score_a = c(10, 40), score_b = c(20, 50)
)

# Scramble multiple variable sets
scramble_variables_rowwise(df, list(
  c("item1", "item2", "item3"),
  c("score_a", "score_b")
))
```

## Datasets

The package includes two research datasets for demonstration:

- **`marp`**: Many Analysts Religion Project data (10,535 participants, 24 countries) - religiosity and well-being measures
- **`williams`**: Risk-taking study data (112 participants) - behavioral measures across ecological conditions

``` r
# Load and explore datasets
data(marp)
data(williams)

# Example: Blind treatment conditions
williams$ecology_masked <- mask_labels(williams$ecology)
head(williams[c("ecology", "ecology_masked")])
```

## Explanation of the package name

Vazul was a Hungarian price in the 11. century. He was blinded by the king to become unfit for the throne. More info: https://en.wikipedia.org/wiki/Vazul

## Documentation

- Package website: https://nthun.github.io/vazul/
- Function help: `?mask_labels`, `?scramble_values`, `?scramble_variables`
- Dataset help: `?marp`, `?williams`

## Authors

- **Tamás Nagy** - Package author and maintainer
- **Alexandra Sarafoglou** - Data contributor and author  
- **Márton Kovács** - Author

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
