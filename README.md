# vazul

<!-- badges: start -->
[![R-CMD-check](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/nthun/vazul/graph/badge.svg)](https://app.codecov.io/gh/nthun/vazul)
<!-- badges: end -->

**vazul** is an R package for data blinding in research contexts. It offers two main approaches to anonymize data while preserving analytical validity: **masking** (replacing values with anonymous labels) and **scrambling** (randomizing the order of existing values).

## Data Blinding Approaches

**Masking** replaces original values with anonymous labels, completely hiding the original information:
```r
treatment <- c("control", "treatment", "control")
mask_labels(treatment)
#> "masked_group_01" "masked_group_02" "masked_group_01"
```

**Scrambling** preserves all original values but randomizes their order:
```r
scramble_values(treatment) 
#> "treatment" "control" "control"  # Same values, different order
```

## Installation

``` r
# install.packages("devtools")
devtools::install_github("nthun/vazul")
```

## Functions

### Masking Functions

Replace categorical values with anonymous labels to completely hide original information.

#### `mask_labels()` - Mask vector values
```r
library(vazul)

# Basic masking
treatment <- c("control", "treatment", "control", "treatment")
set.seed(123)
mask_labels(treatment)
#> "masked_group_01" "masked_group_02" "masked_group_01" "masked_group_02"

# Custom prefix
mask_labels(treatment, prefix = "group_")
#> "group_01" "group_02" "group_01" "group_02"
```

#### `mask_variables()` - Mask data frame columns
```r
df <- data.frame(
  condition = c("A", "B", "A", "B"),
  treatment = c("ctrl", "test", "ctrl", "test"),
  score = c(85, 92, 78, 88)
)

# Mask multiple columns
mask_variables(df, c("condition", "treatment"))

# Use tidyselect helpers
mask_variables(df, where(is.character))
```

#### `mask_variables_rowwise()` & `mask_labels_rowwise()` - Row-level masking
```r
# Consistent masking across rows for categorical data
df |> mask_variables_rowwise(c("condition", "treatment"))
```

### Scrambling Functions  

Randomize the order of values while preserving the original data content.

#### `scramble_values()` - Scramble vector order
```r
# Numeric data
set.seed(123) 
scramble_values(1:5)
#> [1] 3 2 5 4 1

# Categorical data
scramble_values(c("A", "B", "C", "A", "B"))
#> [1] "B" "A" "C" "B" "A"
```

#### `scramble_variables()` - Scramble data frame columns
```r
df <- data.frame(x = 1:6, group = rep(c("A", "B"), each = 3))

# Scramble across entire column
scramble_variables(df, "x")

# Scramble within groups
scramble_variables(df, "x", .groups = "group")

# Using dplyr grouping
library(dplyr)
df |> group_by(group) |> scramble_variables("x")
```

#### `scramble_values_rowwise()` & `scramble_variables_rowwise()` - Row-level scrambling
```r
# Scramble values within each row
df <- data.frame(
  item1 = c(1, 4, 7),
  item2 = c(2, 5, 8), 
  item3 = c(3, 6, 9)
)

df |> scramble_values_rowwise(c("item1", "item2", "item3"))
#>   item1 item2 item3
#> 1     3     1     2
#> 2     5     4     6  
#> 3     8     9     7
```

## Datasets

### MARP Dataset  
Many Analysts Religion Project data: 10,535 participants across 24 countries studying religiosity and well-being.

### Williams Dataset
Experimental study data: 112 participants examining risk-taking behavior under different wealth conditions.

## Explanation of the package name

Vazul was a Hungarian prince in the 11. century. He was blinded by the king to become unfit for the throne. More info: https://en.wikipedia.org/wiki/Vazul

## Documentation

- Function help: `?mask_labels`, `?scramble_values`, etc.
- Package website: https://nthun.github.io/vazul/

## Authors

- **Tamás Nagy** - Package author and maintainer  
- **Alexandra Sarafoglou** - Data contributor and author
- **Márton Kovács** - Author

## License

MIT License - see [LICENSE](LICENSE) file for details.
