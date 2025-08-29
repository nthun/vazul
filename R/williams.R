#' Stereotyping of High-Wealth Individuals in Different Ecologies
#'
#' This dataset originates from a study by Williams et al. investigating whether
#' high-wealth individuals are perceived as having faster life history strategies
#' when they are associated with desperate ecological conditions, compared to hopeful ones.
#'
#' Participants evaluated a target described as a wealthy individual from either a desperate or hopeful ecology.
#' Measures included perceived sexual unrestrictedness, impulsivity, educational and parental investment,
#' and opportunity for long-term planning.
#'
#' @format A data frame with 112 rows and 25 variables:
#' \describe{
#'   \item{subject}{Unique subject ID}
#'   \item{SexUnres_1 to SexUnres_3}{Items measuring perceived sexual unrestrictedness}
#'   \item{SexUnres_4_r, SexUnres_5_r}{Reverse-coded sexual unrestrictedness items}
#'   \item{Impuls_1}{Perceived impulsivity item}
#'   \item{Impuls_2_r, Impul_3_r}{Reverse-coded impulsivity items}
#'   \item{Opport_1 to Opport_5}{Items measuring opportunity for long-term planning}
#'   \item{Opport_6_r}{Reverse-coded opportunity item}
#'   \item{InvEdu_1_r, InvEdu_2_r}{Reverse-coded items measuring perceived educational investment}
#'   \item{InvChild_1}{Perceived parental investment item}
#'   \item{InvChild_2_r}{Reverse-coded parental investment item}
#'   \item{age}{Age of the participant}
#'   \item{gender}{Gender of the participant (1 = Male, 2 = Female)}
#'   \item{ecology}{Experimental condition: "Hopeful" or "Desperate"}
#'   \item{duration_in_seconds}{Duration of the survey in seconds}
#'   \item{attention_1, attention_2}{Attention check items (1 = correct, 0 = failed)}
#' }
#'
#' @source Data based on materials and methods described by Williams et al.
#'
#' @usage data(williams)
#' @keywords datasets
#' @examples
#' data(williams)
#' str(williams)
#' table(williams$ecology)
"williams"
