#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several variables in a data frame.
#' @keywords functions
#' @param data a data frame
#' @param ... <tidy-select> Columns to scramble. Accepts column names, positions, or tidyselect helpers like \code{starts_with()}, \code{contains()}, \code{where()}, etc.
#' @param together logical. If TRUE, variables are scrambled together as a unit per row. Values across different variables are kept intact but assigned to different rows. If FALSE (default), each variable is scrambled independently.
#' @param .groups <tidy-select> Optional grouping columns. Scrambling will be done within each group. Supports same tidyselect syntax as columns.
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

    # Input validation
    stopifnot(is.data.frame(data))

    # Capture all ... arguments as quosures (column sets)
    column_sets <- rlang::enquos(...)

    # If no sets provided, error
    if (length(column_sets) == 0) {
        stop("No columns specified for scrambling.", call. = FALSE)
    }

    # Helper to resolve one column set to column names
    resolve_column_set <- function(set_quo) {
        # Try evaluating as character vector first
        try_char <- tryCatch(
            expr = {
                set <- rlang::eval_tidy(set_quo, data = data)
                if (is.character(set)) {
                    missing <- setdiff(set, names(data))
                    if (length(missing) > 0) {
                        stop("Some column names not found: ", paste(missing, collapse = ", "), call. = FALSE)
                    }
                    return(set)
                } else {
                    NULL
                }
            },
            error = function(e) {
                NULL
            }
        )

        if (!is.null(try_char)) {
            return(try_char)
        }

        # If not character, treat as tidyselect expression
        if (rlang::quo_is_symbol(set_quo) || rlang::quo_is_call(set_quo)) {
            selected <- tidyselect::eval_select(set_quo, data)
            if (length(selected) == 0) {
                stop("No columns selected.", call. = FALSE)
            }
            return(names(data)[selected])
        } else {
            stop("Each column set must be a character vector or tidyselect expression.", call. = FALSE)
        }
    }

    # Resolve all column sets to column names
    all_column_sets <- lapply(column_sets, resolve_column_set)

    # Get all unique columns (for validation and processing)
    all_cols <- unique(unlist(all_column_sets))

    # Handle group selection if provided
    if (!is.null(.groups)) {
        group_indices <- tidyselect::eval_select(rlang::enquo(.groups), data)
        group_cols <- names(data)[group_indices]
    } else {
        group_cols <- NULL
    }

    # Perform scrambling
    if (!is.null(group_cols)) {
        # Add original row order as a column to restore later
        data <- dplyr::mutate(data, .row_id = dplyr::row_number())

        if (together) {
            # Scramble each column set together within groups
            for (col_set in all_column_sets) {
                data <- data |>
                    dplyr::group_by(!!!rlang::syms(group_cols)) |>
                    dplyr::mutate(.scrambled_rows = scramble_values(dplyr::row_number())) |>
                    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(col_set),
                                                .fns = ~ .x[.scrambled_rows]
                    )) |>
                    dplyr::select(-.scrambled_rows) |>
                    dplyr::ungroup()
            }
        } else {
            # Scramble all columns independently within groups
            data <- dplyr::group_by(data, !!!rlang::syms(group_cols)) |>
                dplyr::mutate(
                    dplyr::across(.cols = tidyselect::all_of(all_cols),
                                  .fns = ~ scramble_values(.x)
                    )
                ) |>
                dplyr::ungroup()
        }

        # Restore original row order using .row_id, then drop it
        data <- dplyr::arrange(data, .row_id) |>
            dplyr::select(-.row_id)
    } else {
        if (together) {
            # Non-grouped case with together: scramble each set independently
            for (col_set in all_column_sets) {
                scrambled_indices <- scramble_values(seq_len(nrow(data)))
                data <- dplyr::mutate(
                    data,
                    dplyr::across(
                        .cols = tidyselect::all_of(col_set),
                        .fns = ~ .x[scrambled_indices]
                    )
                )
            }
        } else {
            # Non-grouped case: directly scramble all selected columns independently
            data <- dplyr::mutate(
                data,
                dplyr::across(
                    .cols = tidyselect::all_of(all_cols),
                    .fns = ~ scramble_values(.x)
                )
            )
        }
    }

    # Return modified data
    data
}
