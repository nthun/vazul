#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several selected variables in a data frame simultaneously.
#'   Supports independent scrambling, joint scrambling, and within-group scrambling.
#' @keywords scramble
#' @param data a data frame
#' @param ... Columns to scramble using tidyselect semantics. Each can be:
#'   \itemize{
#'     \item Bare column names (e.g., \code{var1, var2})
#'     \item A tidyselect expression (e.g., \code{starts_with("treat_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#' @param together logical. If `TRUE`, variables are scrambled together as a unit per row.
#'   Values across different variables are kept intact but assigned to different rows.
#'   If `FALSE` (default), each variable is scrambled independently.
#' @param .groups Optional grouping columns. Scrambling will be done within each group.
#'   Supports the same tidyselect syntax as column selection. Grouping columns must not overlap with
#'   the columns selected in \code{...}. If \code{data} is already a grouped \code{dplyr} data frame,
#'   existing grouping is ignored unless \code{.groups} is explicitly provided.
#'
#' @return A data frame with the specified columns scrambled. If grouping is specified, scrambling is done within each group.
#'
#' @seealso \code{\link{scramble_values}} for scrambling a single vector, and
#' \code{\link{scramble_variables_rowwise}} for rowwise scrambling.
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
#' # Using bare names
#' df |> scramble_variables(x, y)
#' # Or using character vector
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
  validate_data_frame(data)
  validate_data_frame_not_empty(data)
  validate_logical_parameter(together, "together")

  # Strip any existing grouping to avoid conflicts
  if (dplyr::is_grouped_df(data)) {
    data <- dplyr::ungroup(data)
  }

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # Resolve column names from ... arguments
  all_col_names <- resolve_all_column_sets(column_sets, data)

  if (!validate_column_selection_not_empty(all_col_names)) {
    return(data)
  }

  # Get column indices from resolved names
  col_indices <- match(all_col_names, names(data))

  # Handle group selection with validation
  group_cols <- resolve_group_columns(
    rlang::enquo(.groups),
    data,
    col_names = all_col_names
  )

  # Perform scrambling (apply grouping once, then reuse the same logic)
  if (!is.null(group_cols)) {
    data <- dplyr::group_by(data, !!!rlang::syms(group_cols))
  }

  if (together) {
    # Scramble row order (within groups if grouped)
    data <- data |>
      dplyr::mutate(.scrambled_rows = scramble_values(dplyr::row_number())) |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(col_indices),
          .fns = ~ .x[.data$.scrambled_rows]
        )
      ) |>
      dplyr::select(-dplyr::all_of(".scrambled_rows"))
  } else {
    # Scramble each selected column independently (within groups if grouped)
    data <- dplyr::mutate(
      data,
      dplyr::across(
        .cols = tidyselect::all_of(col_indices),
        .fns = ~ scramble_values(.x)
      )
    )
  }

  if (!is.null(group_cols)) {
    data <- dplyr::ungroup(data)
  }

  # Return modified data
  return(data)
}
