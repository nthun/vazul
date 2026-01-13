#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several selected variables in a data frame simultaneously. 
#'   Supports independent scrambling, joint scrambling, and within-group scrambling.
#' @keywords scramble functions
#' @param data a data frame
#' @param ... <tidy-select> Columns to scramble. Each can be:
#'   \itemize{
#'     \item A tidyselect expression (e.g., \code{starts_with("treat_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#' @param together logical. If `TRUE`, variables are scrambled together as a unit per row.
#'   Values across different variables are kept intact but assigned to different rows.
#'   If `FALSE` (default), each variable is scrambled independently.
#' @param .groups <tidy-select> Optional grouping columns. Scrambling will be done within each group. 
#'   Supports the same tidyselect syntax as column selection.
#'
#' @return A data frame with the specified columns scrambled. If grouping is specified, scrambling is done within each group.
#' 
#' @seealso \code{\link{scramble_values}} for scrambling a single vector, 
#'
#' @examples
#' df <- data.frame(
#'   x = 1:6,
#'   y = letters[1:6],
#'   group = c("A", "A", "A", "B", "B", "B")
#' )
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
#' data(williams)
#' williams |> scramble_variables(c("ecology", "age"))
#' williams |> scramble_variables(1:5)
#' williams |> scramble_variables(c("ecology", "age"), .groups = "gender")
#' williams |> scramble_variables(c(1, 2), .groups = 3)
#' williams |> scramble_variables(c("ecology", "age"), together = TRUE)
#' williams |> scramble_variables(c("ecology", "age"), .groups = "gender", together = TRUE)
#' 
#' @export
scramble_variables <- function(data, ..., .groups = NULL, together = FALSE) {

    # Input validation
    validate_data_frame(data)

    # Capture all ... arguments as quosures
    column_sets <- rlang::enquos(...)

    # If no sets provided, return data unchanged with warning
    if (length(column_sets) == 0) {
        warning("No columns selected. Returning original data unchanged.",
                call. = FALSE)
        return(data)
    }

    # Resolve column names from ... arguments
    all_col_names <- resolve_all_column_sets(column_sets, data)

    if (length(all_col_names) == 0) {
        warning("No columns selected. Returning original data unchanged.",
                call. = FALSE)
        return(data)
    }

    # Get column indices from resolved names
    col_indices <- match(all_col_names, names(data))

    # Capture original column order
    orig_order <- names(data)

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

        if (together) {
            # Group by the specified columns and scramble row order within each group
            data <- data |>
                dplyr::group_by(!!!rlang::syms(group_cols)) |>
                dplyr::mutate(.scrambled_rows = scramble_values(dplyr::row_number())) |>
                dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(col_indices),
                                            .fns = ~ .x[.scrambled_rows]
                )) |>
                dplyr::select(-.scrambled_rows) |>
                dplyr::ungroup()
        } else {
            # Group by the specified columns and scramble selected columns within each group
            data <- dplyr::group_by(data, !!!rlang::syms(group_cols)) |>
                dplyr::mutate(
                    dplyr::across(.cols = tidyselect::all_of(col_indices),
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
            # Non-grouped case with together: scramble row order
            scrambled_indices <- scramble_values(seq_len(nrow(data)))
            # Re-assign the selected columns based on scrambled row indices
            data <- dplyr::mutate(
                data,
                dplyr::across(
                    .cols = tidyselect::all_of(col_indices),
                    .fns = ~ .x[scrambled_indices]
                )
            )
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
    }

    # Return modified data
    data
}
