#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several variables in a data frame.
#'
#' @param data a data frame
#' @param cols a vector of column names or indices to scramble
#' @param .groups a vector of group names to scramble within groups (default is NULL, meaning no grouping)
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
#'
#' df |> scramble_variables("y", .groups = "group")
#'
#' # Example with the 'williams' dataset
#'
#' data(williams)
#' williams |> scramble_variables(c("ecology", "age"))
#'
#' williams |> scramble_variables(1:5)

#' williams |> scramble_variables(c("ecology", "age"), .groups = "gender")
#'
#' # The function is compatible with column indices
#'
#' williams |> scramble_variables(c(1, 2), .groups = c(3))
#'
#' @export

scramble_variables <- function(data, cols, .groups = NULL) {
    # Input validation
    stopifnot(is.data.frame(data))

    # Capture original column order
    orig_order <- names(data)

    # Handle column selection
    if (is.character(cols)) {
        # Check if column names exist
        missing_cols <- setdiff(cols, names(data))
        if (length(missing_cols) > 0) {
            stop("Some target columns not found in data.", call. = FALSE)
        }
        col_indices <- match(cols, names(data))
    } else if (is.numeric(cols)) {
        # Check if indices are valid
        if (any(cols < 1 | cols > ncol(data))) {
            stop("Some target columns not found in data.", call. = FALSE)
        }
        col_indices <- cols
    } else {
        stop("'cols' must be character vector (column names) or numeric vector (column indices).", call. = FALSE)
    }

    # Handle group selection similarly if provided
    if (!is.null(.groups)) {
        if (is.character(.groups)) {
            missing_groups <- setdiff(.groups, names(data))
            if (length(missing_groups) > 0) {
                stop("Some grouping columns not found in data.", call. = FALSE)
            }
            group_indices <- match(.groups, names(data))
        } else if (is.numeric(.groups)) {
            if (any(.groups < 1 | .groups > ncol(data))) {
                stop("Some grouping columns not found in data.", call. = FALSE)
            }
            group_indices <- .groups
        } else {
            stop("'.groups' must be character vector (column names) or numeric vector (column indices).", call. = FALSE)
        }
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
                across(
                    .cols = {{ col_indices }},
                    .fns = ~ sample(.x)
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
            across(
                .cols = {{ col_indices }},
                .fns = ~ sample(.x)
            )
        )
    }

    # Return modified data
    data
}
