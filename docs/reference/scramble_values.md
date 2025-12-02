# Scramble a vector of values

Scramble a vector of values

## Usage

``` r
scramble_values(x)
```

## Arguments

- x:

  a vector `x`

## Value

the scrambled vector

## Examples

``` r
# Example with character vector
set.seed(123)

x <- letters[1:10]
scramble_values(x)
#>  [1] "c" "j" "b" "h" "f" "i" "a" "g" "e" "d"

#' # Example with numeric vector
nums <- 1:5
scramble_values(nums)
#> [1] 2 3 1 4 5

# Scramble a column in the 'williams' dataset
data(williams)

# Simple scrambling of a single column
set.seed(123)
williams$ecology_scrambled <- scramble_values(williams$ecology)
head(williams[c("ecology", "ecology_scrambled")])
#> # A tibble: 6 × 2
#>   ecology   ecology_scrambled
#>   <chr>     <chr>            
#> 1 Hopeful   Hopeful          
#> 2 Desperate Desperate        
#> 3 Desperate Hopeful          
#> 4 Hopeful   Desperate        
#> 5 Desperate Hopeful          
#> 6 Desperate Desperate        
```
