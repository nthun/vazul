# Mask categorical labels with random labels

Assigns random new labels to each unique value in a character or factor
vector. The purpose is to blind data so analysts are not aware of
treatment allocation or categorical outcomes. Each unique original value
gets a random new label, and the assignment order is randomized to
prevent correspondence with the original order.

## Usage

``` r
mask_labels(x, prefix = "masked_group_")
```

## Arguments

- x:

  a character or factor vector

- prefix:

  character string to use as prefix for masked labels. Default is
  "masked_group\_"

## Value

a vector of the same type as input with masked labels

## See also

[`mask_variables`](https://nthun.github.io/vazul/reference/mask_variables.md)
for masking multiple variables in a data frame,
[`mask_variables_rowwise`](https://nthun.github.io/vazul/reference/mask_variables_rowwise.md)
for rowwise masking, and
[`mask_names`](https://nthun.github.io/vazul/reference/mask_names.md)
for masking variable names.

## Examples

``` r
# Example with character vector
set.seed(123)
treatment <- c("control", "treatment", "control", "treatment")
mask_labels(treatment)
#> [1] "masked_group_01" "masked_group_02" "masked_group_01" "masked_group_02"

# Example with custom prefix
set.seed(456)
condition <- c("A", "B", "C", "A", "B", "C")
mask_labels(condition, prefix = "group_")
#> [1] "group_01" "group_03" "group_02" "group_01" "group_03" "group_02"

# Example with factor vector
set.seed(789)
ecology <- factor(c("Desperate", "Hopeful", "Desperate", "Hopeful"))
mask_labels(ecology)
#> [1] masked_group_01 masked_group_02 masked_group_01 masked_group_02
#> Levels: masked_group_01 masked_group_02

# Using with dataset column
data(williams)
set.seed(123)
williams$ecology_masked <- mask_labels(williams$ecology)
head(williams[c("ecology", "ecology_masked")])
#> # A tibble: 6 × 2
#>   ecology   ecology_masked 
#>   <chr>     <chr>          
#> 1 Hopeful   masked_group_01
#> 2 Desperate masked_group_02
#> 3 Desperate masked_group_02
#> 4 Hopeful   masked_group_01
#> 5 Desperate masked_group_02
#> 6 Desperate masked_group_02
```
