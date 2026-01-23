## ============================================================
## Reproducibility script for vazul package examples
## ============================================================

## Load required packages
library(vazul)
library(dplyr)

## Set seed for reproducibility
set.seed(1037)

## Load included example datasets
data(marp)
data(williams)


## ============================================================
## Masking functions: label-level masking
## ============================================================

## Create a simple treatment vector
treatment <- c(
    "control", "treatment_1", "treatment_2",
    "control", "treatment_1", "treatment_2"
)

## Mask categorical labels with default prefix
mask_labels(treatment)

## Mask labels using a custom prefix
mask_labels(treatment, prefix = "group_")


## ============================================================
## Masking variables in data frames
## ============================================================

## Create a small example dataset from MARP
marp_example <-
    marp |>
    select(subject, country, denomination) |>
    sample_n(6)

## Inspect the original data
marp_example

## Mask selected character columns
marp_example |>
    mask_variables(c("country", "denomination"))

## Mask all character columns using tidyselect
marp_example |>
    mask_variables(where(is.character))


## ============================================================
## Shared masking across variables
## ============================================================

## Example where multiple columns share the same masking scheme
df <- data.frame(
    pre_condition  = c("A", "B", "C", "D"),
    post_condition = c("B", "D", "D", "C"),
    id = c(1, 2, 3, 4)
)

## Inspect original data
df

## Mask conditions jointly across columns
mask_variables(
    df,
    c("pre_condition", "post_condition"),
    across_variables = TRUE
)


## ============================================================
## Row-wise masking across variables
## ============================================================

## Example longitudinal dataset
df <- data.frame(
    id = 1:3,
    wave_1 = c("control", "treatment_1", "treatment_2"),
    wave_2 = c("treatment_1", "treatment_2", "control"),
    wave_3 = c("treatment_2", "control", "treatment_1"),
    wave_4 = c("treatment_2", "control", "treatment_1")
)

## Inspect original data
df

## Apply row-wise masking across waves
mask_variables_rowwise(df, starts_with("wave_"))


## ============================================================
## Masking variable names
## ============================================================

## Inspect original variable names
names(williams)

## Mask impulsivity-related items
williams |>
    mask_names(starts_with("Impul"), prefix = "masked_") |>
    names()


## ============================================================
## Clustered masking of variable names
## ============================================================

## Mask multiple construct-specific variable sets independently
masked_williams <-
    williams |>
    mask_names(starts_with("SexUnres"), prefix = "masked_set_A_") |>
    mask_names(starts_with("Impul"),    prefix = "masked_set_B_") |>
    mask_names(starts_with("Opport"),   prefix = "masked_set_C_") |>
    mask_names(starts_with("InvEdu"),   prefix = "masked_set_D_") |>
    mask_names(starts_with("InvChild"), prefix = "masked_set_E_")

## Inspect masked variable names
names(masked_williams)


## ============================================================
## Example analysis on masked variables
## ============================================================

## Run factor analysis on masked items
masked_williams |>
    select(starts_with("masked_set")) |>
    factanal(factors = 3, rotation = "varimax") |>
    loadings() |>
    print(cutoff = 0.3, sort = TRUE)


## ============================================================
## Scrambling functions
## ============================================================

## Scramble a numeric vector
numbers <- 1:10
scramble_values(numbers)


## ============================================================
## Scrambling variables in data frames
## ============================================================

## Create a small example dataset
williams_example <-
    williams |>
    select(subject, age, ecology) |>
    head()

## Inspect original data
williams_example

## Scramble columns independently
scramble_variables(williams_example, c("age", "ecology"))

## Scramble columns jointly, preserving row-wise associations
scramble_variables(
    williams_example,
    c("age", "ecology"),
    together = TRUE
)


## ============================================================
## Group-wise scrambling
## ============================================================

## Example grouped dataset
df <- data.frame(
    x = 1:6,
    y = letters[1:6],
    group = c("A", "A", "A", "B", "B", "B")
)

## Inspect original data
df

## Scramble values within groups
scramble_variables(df, c("x", "y"), .groups = "group")


## ============================================================
## Row-wise scrambling across variables
## ============================================================

## Example wide-format dataset
df_wide <- data.frame(
    day_1 = c(1, 4, 7),
    day_2 = c(2, 5, 8),
    day_3 = c(3, 6, 9),
    score_a = c(10, 40, 70),
    score_b = c(20, 50, 80),
    id = 1:3
)

## Inspect original data
df_wide

## Scramble values within rows across selected column sets
scramble_variables_rowwise(
    df_wide,
    starts_with("day_"),
    c("score_a", "score_b")
)

sessionInfo()
