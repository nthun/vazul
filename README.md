# vazul

<!-- badges: start -->
[![R-CMD-check](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nthun/vazul/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/nthun/vazul/graph/badge.svg)](https://app.codecov.io/gh/nthun/vazul)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/vazul)](https://CRAN.R-project.org/package=vazul)
<!-- badges: end -->

**vazul** is an R package for analyis blinding in research contexts. It offers two main approaches to anonymize data while preserving analytical validity: **masking** (replacing values with anonymous labels) and **scrambling** (randomizing the order of existing values).

## Analysis Blinding Approaches

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

You can install the released version of vazul from CRAN with:

``` r
install.packages("vazul")
```

Or the development version from GitHub with:
``` r
remotes::install_github("nthun/vazul")
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

The `.across_variables` parameter allows for consistent masking across multiple columns (e.g., longitudinal data in wide format).

```r
df <- data.frame(
  wave_1 = c("A", "B", "A"),
  wave_2 = c("B", "A", "B"),
  score = c(10, 20, 30)
)

# Mask across variables consistently
mask_variables(df, starts_with("wave_"), .across_variables = TRUE)
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

#### Row-wise scrambling: Use `.byrow = TRUE` to shuffle values within each row across the selected columns.

```r
df_items <- data.frame(
  item1 = c(1, 4, 7),
  item2 = c(2, 5, 8), 
  item3 = c(3, 6, 9)
)

# Shuffles values horizontally within each row
scramble_variables(df_items, item1:item3, .byrow = TRUE)
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
- **Márton Kovács** - Author
- **Alexandra Sarafoglou** - Data contributor and author

## Citation

Nagy, T., Kovács, M., & Sarafoglou, A. (2026). vazul: An R package for analysis blinding. Zenodo. https://doi.org/10.5281/zenodo.18269711

## License

MIT License - see [LICENSE](LICENSE) file for details.
