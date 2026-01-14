#' Scramble values across multiple columns rowwise in a data frame
#'
#' For each row, independently shuffle values within the selected columns.
#' All selected columns are combined into a single set and processed together.
#' To scramble different variable groups separately, call the function multiple times.
#' @keywords functions
#' @param data A data frame.
#' @param ... <tidy-select> Columns to scramble. All arguments are combined into
#'   a single set. Each can be:
#'   \itemize{
#'     \item Bare column names (e.g., \code{var1, var2})
#'     \item A tidyselect expression (e.g., \code{starts_with("day_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'   }
#' @return A data frame with values scrambled rowwise within the selected columns.
#'
#' @examples
#' df <- data.frame(
#'   day_1 = c(1, 4, 7),
#'   day_2 = c(2, 5, 8),
#'   day_3 = c(3, 6, 9),
#'   score_a = c(10, 40, 70),
#'   score_b = c(20, 50, 80),
#'   id = 1:3
#' )
#'
#' set.seed(123)
#' # Scramble one set of variables
#' library(dplyr)
#' df |> scramble_variables_rowwise(starts_with("day_"))
#' 
#' # Using character vectors
#' df |> scramble_variables_rowwise(c("day_1", "day_2", "day_3"))
#' 
#' # Scramble multiple sets separately
#' df |>
#'   scramble_variables_rowwise(starts_with("day_")) |>
#'   scramble_variables_rowwise(c("score_a", "score_b"))
#' 
#' @export
scramble_variables_rowwise <- function(data, ...) {
  # Input validation
  validate_data_frame(data)
  validate_data_frame_not_empty(data)

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # Resolve all column sets to column names (combined sets)
  all_col_names <- resolve_all_column_sets(column_sets, data)

  if (!validate_column_selection_not_empty(all_col_names)) {
    return(data)
  }

  # Warn if only one column (no scrambling possible)
  if (length(all_col_names) == 1) {
    warning("Only one column selected. Rowwise scrambling requires at least 2 columns. ",
            "Returning original data unchanged.", call. = FALSE)
    return(data)
  }

  # Check for type compatibility — but allow integer + double
  col_types <- vapply(data[all_col_names], typeof, character(1))
  unique_types <- unique(col_types)
  if (length(unique_types) > 1) {
    # Only warn if types are NOT just integer and double
    if (!setequal(unique_types, c("integer", "double"))) {
      warning(
        "Columns have mixed types: ",
        paste(unique_types, collapse = ", "),
        ". Scrambling may cause coercion.",
        call. = FALSE
      )
    }
  }

  # Copy data
  result <- data

  # Extract selected columns as matrix for row operations
  mat <- as.matrix(data[all_col_names])

  # Scramble each row using scramble_values() function
  scrambled_mat <- t(apply(mat, 1, function(row) {
    row[scramble_values(seq_along(row))]
  }))

  # Assign scrambled values back — preserves types (but R may coerce if types were mixed!)
  result[all_col_names] <- as.data.frame(scrambled_mat)

  return(result)
}
