#' Scramble values rowwise in a data frame
#'
#' For each row, shuffle the values across the selected columns.
#' All selected columns must be of compatible types (e.g., all numeric or all character).
#'
#' @param data a data frame
#' @param cols <tidy-select> Columns to scramble rowwise. Supports helpers like \code{starts_with()}, \code{contains()}, \code{where()}, etc.
#'
#' @return A data frame with values scrambled rowwise across the selected columns.
#'
#' @examples
#' df <- data.frame(
#'   day_1 = c(1, 4, 7),
#'   day_2 = c(2, 5, 8),
#'   day_3 = c(3, 6, 9),
#'   id = 1:3
#' )
#'
#' set.seed(123)
#' df |> scramble_values_rowwise(starts_with("day_"))
#' df |> scramble_values_rowwise(c("day_1", "day_2"))
#'
#' @export
scramble_values_rowwise <- function(data, cols) {

    stopifnot(is.data.frame(data))

    # Capture as quosure and evaluate column selection
    col_indices <- tidyselect::eval_select(rlang::enquo(cols), data)
    col_names <- names(data)[col_indices]

    # Validate at least one column selected
    if (length(col_names) == 0) {
        stop("No columns selected. Please provide valid column names or selection helpers.", call. = FALSE)
    }

    # Warn if only one column (no scrambling possible)
    if (length(col_names) == 1) {
        warning("Only one column selected — no scrambling performed.", call. = FALSE)
        return(data)
    }

    # Copy data
    result <- data

    # Extract selected columns as matrix for row operations
    mat <- as.matrix(data[col_names])

    # Scramble each row using your scramble_values() function
    scrambled_mat <- t(apply(mat, 1, function(row) {
        row[scramble_values(seq_along(row))]
    }))

    # Assign scrambled values back — preserves types
    result[col_names] <- as.data.frame(scrambled_mat)

    result
}
