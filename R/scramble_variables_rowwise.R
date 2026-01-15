#' Scramble values across multiple columns rowwise in a data frame
#'
#' For each row, shuffle values across the selected columns. All selections passed
#' via \code{...} are combined into a single set and scrambled together.
#' To scramble different variable groups separately, call the function multiple times.
#'
#' Rowwise scrambling moves values between columns, so selected columns must be
#' type-compatible. This function requires all selected columns to have the same
#' class (or be an integer/double mix). For factors, the selected columns must
#' also have identical levels.
#' @keywords scramble
#' @param data A data frame.
#' @param ... <tidy-select> Columns to scramble. All arguments are combined into
#'   a single set. Each can be:
#'   \itemize{
#'     \item Bare column names (e.g., \code{var1, var2})
#'     \item A tidyselect expression (e.g., \code{starts_with("day_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'   }
#'   If \code{data} is already a grouped \code{dplyr} data frame, existing grouping
#'   is ignored.
#' @return A data frame with values scrambled rowwise within the selected columns.
#' 
#' @seealso \code{\link{scramble_values}} for scrambling a single vector, and 
#' \code{\link{scramble_variables}} for scrambling multiple variables.
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
#' # Multiple selectors are combined into one set (values can move between day_* and score_*)
#' df |> scramble_variables_rowwise(starts_with("day_"), starts_with("score_"))
#' 
#' @export
scramble_variables_rowwise <- function(data, ...) {
  validate_data_frame(data)
  validate_data_frame_not_empty(data)

  # Strip any existing dplyr grouping to avoid surprises (this function has no grouping semantics)
  if (dplyr::is_grouped_df(data)) {
    data <- dplyr::ungroup(data)
  }

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

  # Validate type/class compatibility.
  # Rowwise scrambling assigns values across columns within each row, so columns must be compatible.
  col_classes <- lapply(data[all_col_names], class)
  col_classes_key <- vapply(col_classes, paste, collapse = "|", character(1))
  same_class <- length(unique(col_classes_key)) == 1

  col_types <- vapply(data[all_col_names], typeof, character(1))
  unique_types <- unique(col_types)
  int_double_mix <- length(unique_types) == 2 && setequal(unique_types, c("integer", "double"))

  if (!same_class && !int_double_mix) {
    stop(
      "Rowwise scrambling requires selected columns to have the same class ",
      "(or be an integer/double mix). Selected classes: ",
      paste(unique(vapply(col_classes, paste, collapse = "/", character(1))), collapse = ", "),
      call. = FALSE
    )
  }

  # Extra safety for factors: levels must match, otherwise assignment will introduce NAs.
  if (inherits(data[[all_col_names[[1]]]], "factor")) {
    levs <- lapply(data[all_col_names], levels)
    if (!all(vapply(levs, identical, logical(1), levs[[1]]))) {
      stop(
        "Rowwise scrambling of factors requires identical levels across selected columns.",
        call. = FALSE
      )
    }
  }

  # Copy data
  result <- data

  cols <- all_col_names
  k <- length(cols)
  n <- nrow(data)

  # Scramble each row by permuting values across the selected columns.
  # This avoids matrix conversion and preserves column classes.
  for (i in seq_len(n)) {
    perm <- scramble_values(seq_len(k))
    row_vals <- lapply(cols, function(nm) data[[nm]][i])
    for (j in seq_len(k)) {
      result[[cols[[j]]]][i] <- row_vals[[perm[[j]]]]
    }
  }

  return(result)
}
