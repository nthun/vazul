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
#' df |> group_by(group) |> scramble_variables("x")
#'
#' # Example with the 'williams' dataset
#'
#' data(williams)
#' williams |> scramble_variables(c("ecology", "age"))
#'
#' williams |> scramble_variables(1:5)

#' williams |>
#'     group_by(gender) |>
#'     scramble_variables(c("ecology", "age")) |>
#'     ungroup()
#'
#' @export

scramble_variables <- function(data, cols, .groups = NULL) {
    # Input validation
    stopifnot(is.data.frame(data))

    # Capture original column order
    orig_order <- names(data)

    # Handle column selection using tidyselect
    cols <- tidyselect::eval_select(enquo(cols), data)

    # Handle group selection similarly if provided
    if (!is.null(.groups)) {
        .groups <- tidyselect::eval_select(enquo(.groups), data)
    }

    # Perform scrambling
    if (!is.null(.groups)) {
        data <- data |>
            dplyr::group_by(dplyr::across(all_of(.groups))) |>
            dplyr::group_modify(function(df, ...) {
                df[cols] <- lapply(df[cols], sample)
                df
            }) |>
            dplyr::ungroup() |>
            dplyr::select(all_of(orig_order))  # Restore original column order
    } else {
        # Non-grouped: directly scramble selected columns
        data[cols] <- lapply(data[cols], sample)
    }

    data
}
