#' MARP: Many Analysts Religion Project dataset
#'
#' Dataset from the Many Analysts Religion Project.
#' Analysts explored relations between religiosity and well-being across multiple countries.
#'
#' @format A data frame with 10,535 rows (participants) and 46 variables:
#' \describe{
#'   \item{subject}{Integer subject identifier (row id in processed file).}
#'   \item{country}{Country of participant (character).}
#'   \item{rel_1 \dots rel_9}{Religiosity items. Many are proportion-style or binary indicators
#'     derived from self-report (values in 0,1 or integer/binary).}
#'   \item{cnorm_1, cnorm_2}{Perceived cultural norm variables: importance of a religious lifestyle
#'     and importance of belief for the average person in the participant's country
#'     (numeric scales).}
#'   \item{wb_gen_1, wb_gen_2}{General well-being items (single-item self-assessments; integer Likert).}
#'   \item{wb_phys_1 \dots wb_phys_7}{Well-being — physical domain items; integer Likert responses (e.g., 1–5).}
#'   \item{wb_psych_1 \dots wb_psych_6}{Well-being — psychological domain items; integer Likert responses.}
#'   \item{wb_soc_1 \dots wb_soc_3}{Well-being — social domain items; integer Likert responses.}
#'   \item{wb_overall_mean}{Participant-level mean across all well-being items (numeric).}
#'   \item{wb_phys_mean}{Mean of the physical well-being items (numeric).}
#'   \item{wb_psych_mean}{Mean of the psychological well-being items (numeric).}
#'   \item{wb_soc_mean}{Mean of the social well-being items (numeric).}
#'   \item{age}{Age of participant in years (integer).}
#'   \item{gender}{Self-reported gender (character). In the notebook gender was recoded to "Male", "Female", "Other".}
#'   \item{ses}{Socio-economic status (numeric/integer scale used in the dataset).}
#'   \item{education}{Education level coded as integers (ordinal).}
#'   \item{ethnicity}{Self-reported ethnicity (character).}
#'   \item{denomination}{Religious denomination (character).}
#'   \item{gdp}{Country-level GDP per capita (numeric).}
#'   \item{gdp_scaled}{Scaled GDP used for models (numeric).}
#'   \item{sample_type}{Sample recruitment type (character; e.g., "online panel", "student sample").}
#'   \item{compensation}{Compensation type (character; e.g., "monetary reward").}
#'   \item{attention_check}{Attention-check variable (integer).}
#' }
#'
#' @source Many Analysts Religion Project. See: https://doi.org/10.1080/2153599X.2023.2254980
#'
#' @usage data(marp)
#' @keywords datasets religion well-being cross-cultural
#' @examples
#' \dontrun{
#' library(dplyr)
#' data(marp)
#' ## quick checks
#' dim(marp)
#' glimpse(marp)
#' ## country-level mean religiosity
#' marp  |>
#'   select(country, starts_with("rel_")) |>
#'   group_by(country) |>
#'   summarise(across(starts_with("rel_"), ~ mean(.x, na.rm = TRUE)))
#' }
"marp"
