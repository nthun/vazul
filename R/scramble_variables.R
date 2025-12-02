#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several variables in a data frame.
#' @keywords functions
#' @param data a data frame
#' @param ... <tidy-select> One or more column sets to scramble. Each can be:
#'   \itemize{
#'     \item A tidyselect expression (e.g., \code{starts_with("day_")})
#'     \item A character vector of column names (e.g., \code{c("x", "y")})
#'     \item Column positions (e.g., \code{1:3})
#'   }
#' @param together logical. If TRUE, variables are scrambled together as a unit per row. Values across different variables are kept intact but assigned to different rows. If FALSE (default), each variable is scrambled independently.
#' @param .groups <tidy-select> Optional grouping columns. Scrambling will be done within each group. Supports same tidyselect syntax as \code{...}.
#'
#' @return A data frame with the specified columns scrambled. If grouping is specified, scrambling is done within each group.
#'
#' @examples
#'
#' df <- data.frame(x = 1:6, y = letters[1:6], group = c("A", "A", "A", "B", "B", "B"))
#'
#' set.seed(123)
#' # Example without grouping. Variables scrambled across the entire data frame.
#' df |> scramble_variables(c("x", "y"))
#'
#' # Example with together = TRUE. Variables scrambled together as a unit per row.
#' df |> scramble_variables(c("x", "y"), together = TRUE)
#'
#' # Example with grouping. Variable only scrambled within groups.
#' df |> scramble_variables("y", .groups = "group")
#'
#' # Example combining grouping and together parameters
#' df |> scramble_variables(c("x", "y"), .groups = "group", together = TRUE)
#'
#' # Example with tidyselect helpers
#' library(dplyr)
#' df |> scramble_variables(starts_with("x"))
#' df |> scramble_variables(where(is.numeric), .groups = "group")
#'
#' # Example with multiple column sets
#' df |> scramble_variables(starts_with("x"), "y")
#'
#' # Example with the 'williams' dataset
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   data(williams, package = "vazul")
#'   williams |> scramble_variables(c("ecology", "age"))
#'   williams |> scramble_variables(1:5)
#'   williams |> scramble_variables(c("ecology", "age"), .groups = "gender")
#'   williams |> scramble_variables(c(1, 2), .groups = c(3))
#'   williams |> scramble_variables(c("ecology", "age"), together = TRUE)
#'   williams |> scramble_variables(c("ecology", "age"), .groups = "gender", together = TRUE)
#' }
#' @export
#'
scramble_variables <- function(data, ..., .groups = NULL, together = FALSE) {
    stopifnot(is.data.frame(data))

    column_sets <- rlang::enquos(...)

    if (length(column_sets) == 0) {
        warning("No columns provided. Returning data unchanged.", call. = FALSE)
        return(data)
    }

    # Process column sets to get all column indices
    col_indices <- resolve_column_indices(column_sets, data)

    if (is.null(col_indices) || length(col_indices) == 0) {
        return(data)
    }

    # Handle group selection if provided
    group_indices <- resolve_group_indices(.groups, data)

    # Perform scrambling
    scramble_data(data, col_indices, group_indices, together)
}


# Perform the actual scrambling operation
#
# @param data A data frame
# @param col_indices Vector of column indices to scramble
# @param group_indices Vector of group column indices (or NULL)
# @param together Logical, whether to scramble together
# @return Data frame with scrambled columns
# @keywords internal
scramble_data <- function(data, col_indices, group_indices, together) {
    if (!is.null(group_indices)) {
        scramble_data_grouped(data, col_indices, group_indices, together)
    } else {
        scramble_data_ungrouped(data, col_indices, together)
    }
}

# Scramble data with grouping
#
# @keywords internal
scramble_data_grouped <- function(data, col_indices, group_indices, together) {
    group_cols <- names(data)[group_indices]

    # Add original row order to restore later
    data <- dplyr::mutate(data, .row_id = dplyr::row_number())

    if (together) {
        data <- data |>
            dplyr::group_by(!!!rlang::syms(group_cols)) |>
            dplyr::mutate(.scrambled_rows = scramble_values(dplyr::row_number())) |>
            dplyr::mutate(dplyr::across(
                .cols = tidyselect::all_of(col_indices),
                .fns = ~ .x[.scrambled_rows]
            )) |>
            dplyr::select(-.scrambled_rows) |>
            dplyr::ungroup()
    } else {
        data <- data |>
            dplyr::group_by(!!!rlang::syms(group_cols)) |>
            dplyr::mutate(dplyr::across(
                .cols = tidyselect::all_of(col_indices),
                .fns = ~ scramble_values(.x)
            )) |>
            dplyr::ungroup()
    }

    # Restore original row order and remove helper column
    data |>
        dplyr::arrange(.row_id) |>
        dplyr::select(-.row_id)
}

# Scramble data without grouping
#
# @keywords internal
scramble_data_ungrouped <- function(data, col_indices, together) {
    if (together) {
        scrambled_indices <- scramble_values(seq_len(nrow(data)))
        dplyr::mutate(
            data,
            dplyr::across(
                .cols = tidyselect::all_of(col_indices),
                .fns = ~ .x[scrambled_indices]
            )
        )
    } else {
        dplyr::mutate(
            data,
            dplyr::across(
                .cols = tidyselect::all_of(col_indices),
                .fns = ~ scramble_values(.x)
            )
        )
    }
}
