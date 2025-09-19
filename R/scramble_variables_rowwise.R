#' Scramble multiple column sets rowwise in a data frame
#'
#' For each row, independently shuffle values within each group of selected columns.
#'
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

    # Capture all ... arguments as quosures
    column_sets <- rlang::enquos(...)

    # If no sets provided, return data unchanged
    if (length(column_sets) == 0) {
        warning("No column sets provided. Returning data unchanged.", call. = FALSE)
        return(data)
    }

    # Helper to handle one set
    scramble_one_set <- function(set_quo) {
        # Try evaluating as character vector first
        try_char <- tryCatch(
            expr = {
                set <- rlang::eval_tidy(set_quo, data = data)
                if (is.character(set)) {
                    missing <- setdiff(set, names(data))
                    if (length(missing) > 0) {
                        warning("Some column names not found: ", paste(missing, collapse = ", "), call. = FALSE)
                        return(NULL)
                    }
                    return(scramble_values_rowwise(data, dplyr::all_of(set))[set])
                } else {
                    # Not character — fall through to tidyselect
                    NULL
                }
            },
            error = function(e) {
                # Not a character vector — fall through to tidyselect
                NULL
            }
        )

        if (!is.null(try_char)) {
            return(try_char)
        }

        # If not character, treat as tidyselect expression
        if (rlang::quo_is_symbol(set_quo) || rlang::quo_is_call(set_quo)) {
            selected <- tryCatch(
                tidyselect::eval_select(set_quo, data),
                error = function(e) {
                    warning("Failed to evaluate column set: ", conditionMessage(e), call. = FALSE)
                    return(NULL)
                }
            )
            if (length(selected) == 0) return(NULL)
            col_names <- names(data)[selected]
            scramble_values_rowwise(data, dplyr::all_of(col_names))[col_names]
        } else {
            warning("Each column set must be a character vector or tidyselect expression.", call. = FALSE)
            return(NULL)
        }
    }

    # Apply to each set
    scrambled_dfs <- lapply(column_sets, scramble_one_set)
    scrambled_dfs <- Filter(Negate(is.null), scrambled_dfs)

    # If nothing was scrambled, return original
    if (length(scrambled_dfs) == 0) {
        return(data)
    }

    # Merge results and preserve column order
    changed_cols <- unlist(lapply(scrambled_dfs, names))
    untouched_cols <- setdiff(names(data), changed_cols)
    result <- cbind(data[untouched_cols], do.call(cbind, scrambled_dfs))
    result[names(data)]  # restore original column order
}
