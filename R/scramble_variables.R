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
#' df <- tibble(x = 1:6, y = letters[1:6], group = c("A", "A", "A", "B", "B", "B"))
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
library(dplyr)

scramble_variables <- function(data, cols, .groups = NULL) {
    stopifnot(is.data.frame(data))

    orig_order <- names(data)

    # Ensure column names
    if (is.numeric(cols)) cols <- names(data)[cols]
    if (!all(cols %in% names(data))) stop("Some target columns not found in data.")

    if (!is.null(.groups)) {
        # Grouped scrambling with native pipe
        data <- data |>
            group_by(across(all_of(.groups))) |>
            group_modify(\(df, ...) {
                df[cols] <- lapply(df[cols], sample)
                df
            }) |>
            ungroup() |>
            select(all_of(orig_order))  # restore original column order
    } else {
        # Non-grouped
        data[cols] <- lapply(data[cols], sample)
    }

    data
}

