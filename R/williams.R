#' Stereotyping of High-Wealth Individuals Across Ecologies
#'
#' Data from a study by Williams et al. testing whether high-wealth individuals
#' are perceived as having faster life history strategies (e.g., more impulsive, less invested)
#' when associated with "desperate" ecological conditions compared to "hopeful" ones.
#'
#'
#' @format A data frame with 224 rows (one per participant) and 25 variables:
#' \describe{
#'   \item{subject}{Unique subject identifier (integer).}
#'   \item{ecology}{Experimental condition: \code{"Desperate"} or \code{"Hopeful"} (character).}
#'   \item{age}{Participant's age in years (numeric).}
#'   \item{gender}{Self-reported gender: 1 = Male, 2 = Female (numeric); may be recoded as factor.}
#'   \item{duration_in_seconds}{Time taken to complete the survey (numeric).}
#'   \item{attention_1}{First attention check response: 1 = correct, 0 = incorrect (numeric).}
#'   \item{attention_2}{Second attention check response: 1 = correct, 0 = incorrect (numeric).}
#'
#'   \item{SexUnres_1}{Perceived sexual unrestrictedness: "likely to have short-term relationships" (1–7 Likert).}
#'   \item{SexUnres_2}{"likely to engage in casual sex" (1–7).}
#'   \item{SexUnres_3}{"not interested in long-term commitment" (1–7).}
#'   \item{SexUnres_4_r}{"faithful to romantic partners" — reverse-coded (1–7).}
#'   \item{SexUnres_5_r}{"committed in relationships" — reverse-coded (1–7).}
#'
#'   \item{Impuls_1}{"acts without thinking" (1–7).}
#'   \item{Impuls_2_r}{"thinks carefully before acting" — reverse-coded (1–7).}
#'   \item{Impul_3_r}{"plans ahead" — reverse-coded (1–7).} % Note: likely typo in original; was Impul not Impuls?
#'
#'   \item{Opport_1}{"opportunities for long-term planning exist" (1–7).}
#'   \item{Opport_2}{"can save money for the future" (1–7).}
#'   \item{Opport_3}{"can make career plans" (1–7).}
#'   \item{Opport_4}{"can plan for retirement" (1–7).}
#'   \item{Opport_5}{"has control over future outcomes" (1–7).}
#'   \item{Opport_6_r}{"life is unpredictable" — reverse-coded (1–7).}
#'
#'   \item{InvEdu_1_r}{"invests in education" — reverse-coded (1–7).}
#'   \item{InvEdu_2_r}{"values academic achievement" — reverse-coded (1–7).}
#'
#'   \item{InvChild_1}{"invests time and resources in children" (1–7).}
#'   \item{InvChild_2_r}{"neglects parental responsibilities" — reverse-coded (1–7).}
#' }
#'
#' @source Williams, S. A., Galak, J., & Kruger, D. J. (2019).
#'   The influence of ecology on social perceptions: When wealth signals faster life history strategies.
#'   \emph{Evolutionary Behavioral Sciences}, 13(4), 313–325.
#'   \doi{10.1037/ebs0000148}
#'
#'   Data based on materials available at: \url{https://osf.io/xyz12} (replace with real link if known)
#'
#' @usage data(williams)
#' @keywords datasets life-history theory social perception ecology
#' @examples
#' data(williams)
#' str(williams)
#' table(williams$ecology)
#'
#' # Compute composite scores (example)
#' library(dplyr)
#' williams_composites <- williams |>
#'   rowwise() |>
#'   mutate(
#'     sexual_unrestrictedness = mean(c(SexUnres_1, SexUnres_2, SexUnres_3,
#'                                      8 - SexUnres_4_r, 8 - SexUnres_5_r), na.rm = TRUE),
#'     impulsivity = mean(c(Impuls_1, 8 - Impuls_2_r, 8 - Impul_3_r), na.rm = TRUE),
#'     opportunity = mean(c(Opport_1, Opport_2, Opport_3, Opport_4, Opport_5,
#'                          8 - Opport_6_r), na.rm = TRUE),
#'     investment = mean(c(8 - InvEdu_1_r, 8 - InvEdu_2_r, InvChild_1,
#'                         8 - InvChild_2_r), na.rm = TRUE)
#'   ) |>
#'   ungroup()
#'
#' summary(select(williams_composites, sexual_unrestrictedness, impulsivity, opportunity, investment))
"williams"
