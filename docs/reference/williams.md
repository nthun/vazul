# Stereotyping of High-Wealth Individuals Across Ecologies

Data from a study by Williams et al. testing whether high-wealth
individuals are perceived as having faster life history strategies
(e.g., more impulsive, less invested) when associated with "desperate"
ecological conditions compared to "hopeful" ones.

## Usage

``` r
data(williams)
```

## Format

A data frame with 224 rows (one per participant) and 25 variables:

- subject:

  Unique subject identifier (integer).

- ecology:

  Experimental condition: `"Desperate"` or `"Hopeful"` (character).

- age:

  Participant's age in years (numeric).

- gender:

  Self-reported gender: 1 = Male, 2 = Female (numeric); may be recoded
  as factor.

- duration_in_seconds:

  Time taken to complete the survey (numeric).

- attention_1:

  First attention check response: 1 = correct, 0 = incorrect (numeric).

- attention_2:

  Second attention check response: 1 = correct, 0 = incorrect (numeric).

- SexUnres_1:

  Perceived sexual unrestrictedness: "likely to have short-term
  relationships" (1–7 Likert).

- SexUnres_2:

  "likely to engage in casual sex" (1–7).

- SexUnres_3:

  "not interested in long-term commitment" (1–7).

- SexUnres_4_r:

  "faithful to romantic partners" — reverse-coded (1–7).

- SexUnres_5_r:

  "committed in relationships" — reverse-coded (1–7).

- Impuls_1:

  "acts without thinking" (1–7).

- Impuls_2_r:

  "thinks carefully before acting" — reverse-coded (1–7).

- Impul_3_r:

  "plans ahead" — reverse-coded (1–7).

- Opport_1:

  "opportunities for long-term planning exist" (1–7).

- Opport_2:

  "can save money for the future" (1–7).

- Opport_3:

  "can make career plans" (1–7).

- Opport_4:

  "can plan for retirement" (1–7).

- Opport_5:

  "has control over future outcomes" (1–7).

- Opport_6_r:

  "life is unpredictable" — reverse-coded (1–7).

- InvEdu_1_r:

  "invests in education" — reverse-coded (1–7).

- InvEdu_2_r:

  "values academic achievement" — reverse-coded (1–7).

- InvChild_1:

  "invests time and resources in children" (1–7).

- InvChild_2_r:

  "neglects parental responsibilities" — reverse-coded (1–7).

## Source

Williams, S. A., Galak, J., & Kruger, D. J. (2019). The influence of
ecology on social perceptions: When wealth signals faster life history
strategies. *Evolutionary Behavioral Sciences*, 13(4), 313–325.
[doi:10.1037/ebs0000148](https://doi.org/10.1037/ebs0000148)

Data based on materials available at: <https://osf.io/xyz12> (replace
with real link if known)

## Examples

``` r
data(williams)
str(williams)
#> spc_tbl_ [112 × 25] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#>  $ subject            : chr [1:112] "A30MP4LXV4MIFD" "A16X5FB3HAFCKN" "A1E9D1OT9VJYDZ" "A16FPOYD7566WI" ...
#>  $ SexUnres_1         : num [1:112] 5 7 2 5 5 5 5 4 6 4 ...
#>  $ SexUnres_2         : num [1:112] 3 7 4 4 5 6 6 6 5 4 ...
#>  $ SexUnres_3         : num [1:112] 2 7 6 5 6 6 5 5 6 4 ...
#>  $ SexUnres_4_r       : num [1:112] 3 4 3 3 3 3 3 2 3 1 ...
#>  $ SexUnres_5_r       : num [1:112] 2 2 3 3 3 2 4 3 2 1 ...
#>  $ Impuls_1           : num [1:112] 3 6 3 4 5 5 5 6 6 1 ...
#>  $ Impuls_2_r         : num [1:112] 3 2 3 4 2 3 3 2 3 1 ...
#>  $ Impul_3_r          : num [1:112] 2 1 3 4 4 3 2 3 2 1 ...
#>  $ Opport_1           : num [1:112] 1 5 3 4 4 4 5 5 5 1 ...
#>  $ Opport_2           : num [1:112] 2 7 5 4 6 3 5 4 4 4 ...
#>  $ Opport_3           : num [1:112] 2 7 5 4 4 4 6 5 5 1 ...
#>  $ Opport_4           : num [1:112] 3 6 3 5 4 6 5 5 4 4 ...
#>  $ Opport_5           : num [1:112] 1 6 3 4 5 4 6 6 4 1 ...
#>  $ Opport_6_r         : num [1:112] 3 2 3 4 3 3 2 2 3 2 ...
#>  $ InvEdu_1_r         : num [1:112] 2 3 2 3 4 3 3 3 3 2 ...
#>  $ InvEdu_2_r         : num [1:112] 3 2 3 4 3 4 2 3 4 1 ...
#>  $ InvChild_1         : num [1:112] 2 5 6 5 5 5 6 5 6 1 ...
#>  $ InvChild_2_r       : num [1:112] 3 2 3 4 3 4 2 4 3 2 ...
#>  $ age                : num [1:112] 34 30 40 35 26 33 33 30 48 33 ...
#>  $ gender             : num [1:112] 1 1 1 2 2 2 2 1 2 2 ...
#>  $ ecology            : chr [1:112] "Hopeful" "Desperate" "Desperate" "Hopeful" ...
#>  $ duration_in_seconds: num [1:112] 164 100 47 31 40 32 98 34 32 87 ...
#>  $ attention_1        : num [1:112] 1 1 1 1 1 1 1 1 1 0 ...
#>  $ attention_2        : num [1:112] 1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, "spec")=List of 3
#>   ..$ cols   :List of 25
#>   .. ..$ subject            : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ SexUnres_1         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ SexUnres_2         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ SexUnres_3         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ SexUnres_4_r       : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ SexUnres_5_r       : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Impuls_1           : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Impuls_2_r         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Impul_3_r          : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Opport_1           : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Opport_2           : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Opport_3           : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Opport_4           : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Opport_5           : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ Opport_6_r         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ InvEdu_1_r         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ InvEdu_2_r         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ InvChild_1         : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ InvChild_2_r       : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ age                : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ gender             : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ ecology            : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
#>   .. ..$ duration_in_seconds: list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ attention_1        : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   .. ..$ attention_2        : list()
#>   .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
#>   ..$ default: list()
#>   .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
#>   ..$ delim  : chr ";"
#>   ..- attr(*, "class")= chr "col_spec"
#>  - attr(*, "problems")=<externalptr> 
table(williams$ecology)
#> 
#> Desperate   Hopeful 
#>        56        56 

# Compute composite scores (example)
library(dplyr)
williams_composites <- williams |>
  rowwise() |>
  mutate(
    sexual_unrestrictedness = mean(c(SexUnres_1, SexUnres_2, SexUnres_3,
                                     8 - SexUnres_4_r, 8 - SexUnres_5_r), na.rm = TRUE),
    impulsivity = mean(c(Impuls_1, 8 - Impuls_2_r, 8 - Impul_3_r), na.rm = TRUE),
    opportunity = mean(c(Opport_1, Opport_2, Opport_3, Opport_4, Opport_5,
                         8 - Opport_6_r), na.rm = TRUE),
    investment = mean(c(8 - InvEdu_1_r, 8 - InvEdu_2_r, InvChild_1,
                        8 - InvChild_2_r), na.rm = TRUE)
  ) |>
  ungroup()

summary(select(williams_composites, sexual_unrestrictedness, impulsivity, opportunity, investment))
#>  sexual_unrestrictedness  impulsivity     opportunity      investment   
#>  Min.   :3.000           Min.   :3.000   Min.   :1.667   Min.   :2.000  
#>  1st Qu.:4.200           1st Qu.:4.667   1st Qu.:2.833   1st Qu.:4.500  
#>  Median :4.800           Median :5.000   Median :4.167   Median :5.000  
#>  Mean   :4.802           Mean   :5.027   Mean   :4.116   Mean   :4.951  
#>  3rd Qu.:5.400           3rd Qu.:5.667   3rd Qu.:5.167   3rd Qu.:5.500  
#>  Max.   :6.600           Max.   :7.000   Max.   :6.667   Max.   :6.500  
```
