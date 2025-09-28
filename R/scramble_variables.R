#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several variables in a data frame.
#'
#' @param data a data frame
#' @param cols <tidy-select> Columns to scramble. Accepts column names, positions, or tidyselect helpers like \code{starts_with()}, \code{contains()}, \code{where()}, etc.
#' @param .groups <tidy-select> Optional grouping columns. Scrambling will be done within each group. Supports same tidyselect syntax as \code{cols}.
#' @param together logical. If \code{TRUE}, each row of variables should be scrambled
#'   as a unit. The values across different variables are kept intact, but are
#'   assigned to a different row. Defaults to \code{FALSE}.
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
#' # Example with together = TRUE. Entire rows scrambled as units.
#' df |> scramble_variables(c("x", "y"), together = TRUE)
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
scramble_variables <- function(data, cols, .groups = NULL, together = FALSE) {

    # Input validation
    stopifnot(is.data.frame(data))
    
    # Validate together parameter
    if (is.null(together)) {
        stop("Parameter 'together' cannot be NULL. ",
             "Please provide a logical value.", call. = FALSE)
    }
    
    if (!is.logical(together) || length(together) != 1) {
        stop("Parameter 'together' must be a single logical value ",
             "(TRUE or FALSE).", call. = FALSE)
    }

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
    if (together) {
        # When together = TRUE, scramble entire rows as units
        
        if (!is.null(group_indices)) {
            # Grouped case with together = TRUE
            group_cols <- names(data)[group_indices]
            
            # Create temporary data for scrambling within groups
            temp_data <- data
            selected_cols <- names(data)[col_indices]
            
            # For each group, scramble the selected columns together
            unique_groups <- unique(data[group_cols])
            for (i in seq_len(nrow(unique_groups))) {
                # Find rows belonging to this group
                group_condition <- rep(TRUE, nrow(data))
                for (gc in group_cols) {
                    group_condition <- group_condition & (data[[gc]] == unique_groups[[gc]][i])
                }
                group_rows <- which(group_condition)
                
                if (length(group_rows) > 1) {
                    # Scramble the row indices within this group
                    scrambled_indices <- sample(group_rows)
                    
                    # Apply scrambling to selected columns for this group
                    for (col in selected_cols) {
                        temp_data[group_rows, col] <- data[scrambled_indices, col]
                    }
                }
            }
            data <- temp_data
            
        } else {
            # Non-grouped case with together = TRUE
            # Simply scramble the row indices for selected columns
            scrambled_indices <- sample(seq_len(nrow(data)))
            
            # Apply scrambled indices to selected columns
            selected_cols <- names(data)[col_indices]
            for (col in selected_cols) {
                data[[col]] <- data[[col]][scrambled_indices]
            }
        }
        
    } else if (!is.null(group_indices)) {
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
