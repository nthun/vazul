#' Scrambling variables rowwise in a data frame
#'
#' Scramble the values of defined variable sets rowwise in a data frame.
#' For each row, values within each variable set are shuffled while keeping
#' the values within the same row.
#'
#' @param data a data frame
#' @param variable_sets Either a vector of column names/indices (supports tidyselect
#'   helpers like \code{starts_with()}, \code{contains()}, \code{where()}, etc.), 
#'   or a list of character/numeric vectors where each vector contains column 
#'   names/indices that should be scrambled together within each row.
#' @param .groups <tidy-select> Optional grouping columns. Scrambling will be 
#'   done within each group. Supports same tidyselect syntax as \code{variable_sets}.
#'
#' @return A data frame with the specified variable sets scrambled rowwise. 
#'   If grouping is specified, scrambling is done within each group.
#'
#' @examples
#'
#' # Create example data with multiple variable sets
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
#' # Scramble a single set of variables rowwise
#' df |> scramble_variables_rowwise(c("day_1", "day_2", "day_3"))
#'
#' # Scramble multiple sets of variables
#' df |> scramble_variables_rowwise(list(
#'   c("day_1", "day_2", "day_3"),
#'   c("score_a", "score_b")
#' ))
#'
#' # Using tidyselect helpers
#' library(dplyr)
#' df |> scramble_variables_rowwise(starts_with("day_"))
#'
#' # Multiple sets with character vectors (tidyselect helpers in list not yet supported)
#' df |> scramble_variables_rowwise(list(
#'   c("day_1", "day_2", "day_3"),
#'   c("score_a", "score_b")
#' ))
#'
#' # Example with the 'williams' dataset - scramble related measures rowwise
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   data(williams)
#'   
#'   # Scramble sexual unrestrictedness items within each row
#'   williams |> scramble_variables_rowwise(
#'     c("SexUnres_1", "SexUnres_2", "SexUnres_3")
#'   )
#'   
#'   # Scramble multiple construct sets using character vectors
#'   williams |> scramble_variables_rowwise(list(
#'     c("SexUnres_1", "SexUnres_2", "SexUnres_3"),
#'     c("Impuls_1", "Impuls_2_r", "Impul_3_r")
#'   ))
#'   
#'   # With grouping by ecology condition
#'   williams |> scramble_variables_rowwise(
#'     starts_with("SexUnres_"), 
#'     .groups = "ecology"
#'   )
#' }
#'
#' @export
#'
scramble_variables_rowwise <- function(data, variable_sets, .groups = NULL) {
  
  # Input validation
  stopifnot(is.data.frame(data))
  
  # Handle group selection if provided
  if (!is.null(.groups)) {
    group_indices <- tidyselect::eval_select(rlang::enquo(.groups), data)
    group_cols <- names(data)[group_indices]
  } else {
    group_indices <- NULL
    group_cols <- NULL
  }
  
  # Handle variable_sets - if it's not a list, make it a single-element list
  if (!is.list(variable_sets)) {
    # Check for empty inputs early
    if (length(variable_sets) == 0) {
      stop("variable_sets cannot be empty. Please provide at least one set of variables.", call. = FALSE)
    }
    # Use the same approach as scramble_variables
    col_indices <- tidyselect::eval_select(rlang::enquo(variable_sets), data)
    resolved_sets <- list(names(data)[col_indices])
  } else {
    # variable_sets is a list - this is more complex since we can't easily 
    # capture each element as a quosure from within purrr::map
    # For now, we'll require that list elements are character vectors
    if (length(variable_sets) == 0) {
      stop("variable_sets cannot be empty. Please provide at least one set of variables.", call. = FALSE)
    }
    resolved_sets <- purrr::map(variable_sets, function(var_set) {
      if (is.character(var_set) || is.numeric(var_set)) {
        # Direct column names or indices
        if (is.numeric(var_set)) {
          names(data)[var_set]
        } else {
          var_set
        }
      } else {
        stop("When variable_sets is a list, each element must be a character vector of column names or numeric vector of column indices.", call. = FALSE)
      }
    })
  }
  
  # Validate that variable_sets is not empty after processing
  if (length(resolved_sets) == 0) {
    stop("variable_sets cannot be empty. Please provide at least one set of variables.", call. = FALSE)
  }
  
  # Validate that all sets have at least one column
  purrr::iwalk(resolved_sets, function(col_names, idx) {
    if (length(col_names) == 0) {
      stop("Variable set ", idx, " resolved to zero columns. Please check your selection.", call. = FALSE)
    }
    # Check that all columns exist in the data
    missing_cols <- setdiff(col_names, names(data))
    if (length(missing_cols) > 0) {
      stop("Variable set ", idx, " contains non-existent columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  })
  
  # Apply scrambling
  if (!is.null(group_indices)) {
    # Store original column order
    original_names <- names(data)
    
    # Add original row order as a column to restore later
    data <- dplyr::mutate(data, .row_id = dplyr::row_number())
    
    # Group by the specified columns and apply rowwise scrambling within each group
    data <- data |>
      dplyr::group_by(!!!rlang::syms(group_cols)) |>
      dplyr::group_modify(~ {
        # Apply scrambling to each row within the group directly
        for (i in seq_len(nrow(.x))) {
          # For each variable set, scramble within this row
          for (col_set in resolved_sets) {
            if (length(col_set) > 1) {
              # Get values for this row and variable set
              row_values <- .x[i, col_set, drop = FALSE]
              # Scramble indices
              scrambled_indices <- scramble_values(seq_along(col_set))
              # Reorder values
              scrambled_values <- row_values[, scrambled_indices, drop = FALSE]
              # Put back
              .x[i, col_set] <- scrambled_values
            }
          }
        }
        .x
      }) |>
      dplyr::ungroup()
    
    # Restore original row order using .row_id, then drop it
    result <- dplyr::arrange(data, .row_id) |>
      dplyr::select(-.row_id)
    
    # Restore original column order
    result <- result[original_names]
  } else {
    # No grouping - apply scrambling to each row directly
    result <- data
    for (i in seq_len(nrow(data))) {
      # For each variable set, scramble within this row
      for (col_set in resolved_sets) {
        if (length(col_set) > 1) {
          # Get values for this row and variable set
          row_values <- data[i, col_set, drop = FALSE]
          # Scramble indices
          scrambled_indices <- scramble_values(seq_along(col_set))
          # Reorder values
          scrambled_values <- row_values[, scrambled_indices, drop = FALSE]
          # Put back
          result[i, col_set] <- scrambled_values
        }
      }
    }
  }
  
  result
}