#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several variables in a data frame.
#'
#' @param data a data frame
#' @param cols <tidy-select> Columns to scramble. Accepts column names, positions, or tidyselect helpers like \code{starts_with()}, \code{contains()}, \code{where()}, etc.
#'
#' @return A data frame with the specified columns scrambled.
#'
#' @examples
#'
#' df <- data.frame(x = 1:6, y = letters[1:6], group = c("A", "A", "A", "B", "B", "B"))
#'
#' set.seed(123)
#' # Example without grouping. Variables scrambled across the entire data frame.
#' df |> scramble_variables(c("x", "y"))
#'
#' # Example with tidyselect helpers
#' library(dplyr)
#' df |> scramble_variables(starts_with("x"))
#' df |> scramble_variables(where(is.numeric))
#'
#' # Example with the 'williams' dataset
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   data(williams, package = "dplyr")
#'   williams |> scramble_variables(c("ecology", "age"))
#'   williams |> scramble_variables(1:5)
#' }
#'
#' @export
#'
scramble_variables <- function(data, cols) {

    # Input validation
    stopifnot(is.data.frame(data))

    # Capture original column order
    orig_order <- names(data)

    # Handle column selection using tidyselect — throws its own meaningful errors
    col_indices <- tidyselect::eval_select(rlang::enquo(cols), data)

    # Perform scrambling - directly scramble selected columns
    data <- dplyr::mutate(
        data,
        dplyr::across(
            .cols = tidyselect::all_of(col_indices),
            .fns = ~ scramble_values(.x)
        )
    )

    # Return modified data
    data
}
