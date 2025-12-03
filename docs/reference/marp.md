# MARP: Many Analysts Religion Project Dataset

A cross-cultural dataset from the Many-Analysts Religion Project (MARP),
which investigated the relationship between religiosity and well-being
across 24 countries and diverse religious traditions.

## Usage

``` r
data(marp)
```

## Format

A data frame with 10,535 rows (participants) and 48 variables:

- subject:

  Unique subject identifier (integer).

- country:

  Country of residence (character string).

- rel_1:

  Importance of religion in daily life (0–10 scale).

- rel_2:

  Frequency of religious service attendance (ordinal).

- rel_3:

  Self-rated religiosity (0–10 scale).

- rel_4:

  Belief in God (binary: yes/no).

- rel_5:

  Prayer frequency (ordinal).

- rel_6:

  Bible/study frequency (ordinal).

- rel_7:

  Religious upbringing (binary: yes/no).

- rel_8:

  Current religious denomination (categorical).

- rel_9:

  Change in religiosity over lifetime (ordinal).

- cnorm_1:

  Perceived cultural norm: importance of religious lifestyle for average
  person in country (0–10).

- cnorm_2:

  Perceived cultural norm: importance of belief in God for average
  person in country (0–10).

- wb_gen_1:

  Overall life satisfaction (1–5 Likert).

- wb_gen_2:

  Overall happiness (1–5 Likert).

- wb_phys_1:

  Energy level (1–5).

- wb_phys_2:

  Sleep quality (1–5).

- wb_phys_3:

  Appetite (1–5).

- wb_phys_4:

  Physical pain/discomfort (1–5).

- wb_phys_5:

  General health (1–5).

- wb_phys_6:

  Exercise frequency (1–5).

- wb_phys_7:

  Illness burden (1–5).

- wb_psych_1:

  Positive affect (1–5).

- wb_psych_2:

  Negative affect (reverse coded; 1–5).

- wb_psych_3:

  Meaning in life (1–5).

- wb_psych_4:

  Purpose in life (1–5).

- wb_psych_5:

  Hopefulness (1–5).

- wb_psych_6:

  Anxiety (reverse coded; 1–5).

- wb_soc_1:

  Social support (1–5).

- wb_soc_2:

  Loneliness (reverse coded; 1–5).

- wb_soc_3:

  Community belonging (1–5).

- wb_overall_mean:

  Mean of all well-being items (numeric).

- wb_phys_mean:

  Mean of physical well-being items (numeric).

- wb_psych_mean:

  Mean of psychological well-being items (numeric).

- wb_soc_mean:

  Mean of social well-being items (numeric).

- age:

  Age in years (integer).

- gender:

  Self-reported gender (character: e.g., "Male", "Female", "Other").

- ses:

  Socioeconomic status composite (numeric).

- education:

  Highest education level completed (ordinal integer).

- ethnicity:

  Self-reported ethnicity (character).

- denomination:

  Religious denomination (character).

- gdp:

  GDP per capita (PPP, USD) for country (numeric).

- gdp_scaled:

  Scaled GDP (mean = 0, sd = 1) used in analyses (numeric).

- sample_type:

  Recruitment method: e.g., "online panel", "student sample"
  (character).

- compensation:

  Type of compensation: e.g., "monetary", "entry into lottery"
  (character).

- attention_check:

  Score on embedded attention check task (integer).

## Source

Hoogeveen, S., Sarafoglou, A., Aczel, B., et al. (2022). A many-analysts
approach to the relation between religiosity and well-being. *Religion,
Brain & Behavior*.
[doi:10.1080/2153599X.2023.2254980](https://doi.org/10.1080/2153599X.2023.2254980)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
data(marp)
# Dimensions
dim(marp)
# Quick overview
dplyr::glimpse(marp)
# Mean religiosity by country
marp |>
  dplyr::select(country, starts_with("rel_")) |>
  dplyr::group_by(country) |>
  dplyr::summarise(
    across(starts_with("rel_"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
} # }
```
