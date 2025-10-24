#' Scramble multiple column sets rowwise in a data frame
#'
#' For each row, independently shuffle values within each group of selected columns.
#' @keywords functions
#' @param data A data frame.
#' @param ... <tidy-select> One or more column sets. Each can be:
#'   \itemize{
#'     \item A tidyselect expression (e.g., \code{starts_with("day_")})
#'     \item A character vector of column names (e.g., \code{c("day_1", "day_2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#' @return A data frame with values scrambled rowwise within each selected column set.
#'
#' @examples
#' df <- data.frame(
#'   day_1 = c(1, 4, 7),
#'   day_2 = c(2, 5, 8),
#'   day_3 = c(3, 6, 9),
#'   score_a = c(10, 40, 70),
#'   score_b = c(20, 50, 80),
#'   id = 1:3
#' )
#'
#' set.seed(123)
#' df |> scramble_variables_rowwise(starts_with("day_"))
#' df |> scramble_variables_rowwise(c("day_1", "day_2"))
#' df |> scramble_variables_rowwise(
#'   starts_with("day_"),
#'   c("score_a", "score_b")
#' )
#' @export
scramble_variables_rowwise <- function(data, ...) {
    stopifnot(is.data.frame(data))

    column_sets <- rlang::enquos(...)

    if (length(column_sets) == 0) {
        warning("No column sets provided. Returning data unchanged.", call. = FALSE)
        return(data)
    }

    # Process one column set and return blinded columns or NULL
    process_column_set <- function(set_quo) {
        # Try to evaluate as character vector
        char_result <- try_evaluate_as_character(set_quo, data, scramble_values_rowwise)
        if (!is.null(char_result)) {
            return(char_result)
        }

        # Otherwise, treat as tidyselect expression
        evaluate_as_tidyselect(set_quo, data, scramble_values_rowwise)
    }

    # Process all column sets and combine results
    column_sets |>
        resolve_and_blind_columns(data, scramble_values_rowwise) |>
        merge_blinded_columns_into(data)
}
