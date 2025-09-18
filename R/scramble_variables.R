#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several variables in a data frame.
#'
#' @param data a data frame
#' @param cols <tidy-select> Columns to scramble. Accepts column names, positions, or tidyselect helpers like \code{starts_with()}, \code{contains()}, \code{where()}, etc.
#' @param .groups <tidy-select> Optional grouping columns. Scrambling will be done within each group. Supports same tidyselect syntax as \code{cols}.
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
#' # Example with grouping. Variable only scrambled within groups.
#' df |> scramble_variables("y", .groups = "group")
#'
#' # Example with tidyselect helpers
#' library(dplyr)
#' df |> scramble_variables(starts_with("x"))
#' df |> scramble_variables(where(is.numeric), .groups = "group")
#'
#' # Example with the 'williams' dataset
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   data(williams, package = "dplyr")
#'   williams |> scramble_variables(c("ecology", "age"))
#'   williams |> scramble_variables(1:5)
#'   williams |> scramble_variables(c("ecology", "age"), .groups = "gender")
#'   williams |> scramble_variables(c(1, 2), .groups = c(3))
#' }
#'
#' @export
#'
scramble_variables <- function(data, cols, .groups = NULL) {

    # Input validation
    stopifnot(is.data.frame(data))

    # Capture original column order
    orig_order <- names(data)

    # Handle column selection using tidyselect — throws its own meaningful errors
    col_indices <- tidyselect::eval_select(rlang::enquo(cols), data)

    # Handle group selection similarly if provided
    if (!is.null(.groups)) {
        group_indices <- tidyselect::eval_select(rlang::enquo(.groups), data)
    } else {
        group_indices <- NULL
    }

    # Perform scrambling
    if (!is.null(group_indices)) {
        # Extract group column names
        group_cols <- names(data)[group_indices]

        # Add original row order as a column to restore later
        data <- dplyr::mutate(data, .row_id = dplyr::row_number())

        # Group by the specified columns and scramble selected columns within each group
        data <- dplyr::group_by(data, !!!rlang::syms(group_cols))  |>
            dplyr::mutate(
                dplyr::across(
                    .cols = tidyselect::all_of(col_indices),
                    .fns = ~ scramble_values(.x)
                )
            ) |>
            dplyr::ungroup()

        # Restore original row order using .row_id, then drop it
        data <- dplyr::arrange(data, .row_id) |>
            dplyr::select(-.row_id)
    } else {
        # Non-grouped case: directly scramble selected columns
        data <- dplyr::mutate(
            data,
            dplyr::across(
                .cols = tidyselect::all_of(col_indices),
                .fns = ~ scramble_values(.x)
            )
        )
    }

    # Return modified data
    data
}
