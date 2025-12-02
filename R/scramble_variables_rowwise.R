#' Scramble multiple column sets rowwise in a data frame
#'
#' For each row, independently shuffle values within each group of selected columns.
#' @keywords functions
#' @param data A data frame.
#' @param ... <tidy-select> One or more column sets. Each can be:
#'   \itemize{
#'     \item A tidyselect expression (e.g., \code{starts_with("day_")})
#'     \item A character vector of column names (e.g., \code{c("day_1", "day_2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#' @return A data frame with values scrambled rowwise within each selected column set.
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
#' df |> scramble_variables_rowwise(starts_with("day_"))
#' df |> scramble_variables_rowwise(c("day_1", "day_2"))
#' df |> scramble_variables_rowwise(
#'   starts_with("day_"),
#'   c("score_a", "score_b")
#' )
#' @export
scramble_variables_rowwise <- function(data, ...) {
  stopifnot(is.data.frame(data))

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # If no sets provided, return data unchanged
  if (length(column_sets) == 0) {
    warning("No column sets provided. Returning data unchanged.", call. = FALSE)
    return(data)
  }

  # Internal helper: scramble values rowwise for a single set of columns
  scramble_rowwise_internal <- function(df, col_names) {
    # Validate at least one column selected
    if (length(col_names) == 0) {
      stop("No columns selected. Please provide valid column names or selection helpers.", call. = FALSE)
    }

    # Warn if only one column (no scrambling possible)
    if (length(col_names) == 1) {
      warning("Only one column selected - no scrambling performed.", call. = FALSE)
      return(df)
    }

    # Check for type compatibility — but allow integer + double
    col_types <- vapply(df[col_names], typeof, character(1))
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
    result <- df

    # Extract selected columns as matrix for row operations
    mat <- as.matrix(df[col_names])

    # Scramble each row using scramble_values() function
    scrambled_mat <- t(apply(mat, 1, function(row) {
      row[scramble_values(seq_along(row))]
    }))

    # Assign scrambled values back — preserves types (but R may coerce if types were mixed!)
    result[col_names] <- as.data.frame(scrambled_mat)

    result
  }

  # Helper to handle one set
  scramble_one_set <- function(set_quo) {
    # Try evaluating as character vector first
    try_char <- tryCatch(
      expr = {
        set <- rlang::eval_tidy(set_quo, data = data)
        if (is.character(set)) {
          missing <- setdiff(set, names(data))
          if (length(missing) > 0) {
            warning("Some column names not found: ", paste(missing, collapse = ", "), call. = FALSE)
            return(NULL)
          }
          return(scramble_rowwise_internal(data, set)[set])
        } else {
          # Not character — fall through to tidyselect
          NULL
        }
      },
      error = function(e) {
        # Not a character vector — fall through to tidyselect
        NULL
      }
    )

    if (!is.null(try_char)) {
      return(try_char)
    }

    # If not character, treat as tidyselect expression
    if (rlang::quo_is_symbol(set_quo) || rlang::quo_is_call(set_quo)) {
      selected <- tryCatch(
        tidyselect::eval_select(set_quo, data),
        error = function(e) {
          warning("Failed to evaluate column set: ", conditionMessage(e), call. = FALSE)
          return(NULL)
        }
      )
      if (length(selected) == 0) return(NULL)
      col_names <- names(data)[selected]
      scramble_rowwise_internal(data, col_names)[col_names]
    } else {
      warning("Each column set must be a character vector or tidyselect expression.", call. = FALSE)
      return(NULL)
    }
  }

  # Apply to each set
  scrambled_dfs <- lapply(column_sets, scramble_one_set)
  scrambled_dfs <- Filter(Negate(is.null), scrambled_dfs)

  # If nothing was scrambled, return original
  if (length(scrambled_dfs) == 0) {
    return(data)
  }

  # Start with original data to preserve class
  result <- data

  # Use functional approach to assign scrambled columns back
  # This preserves the original data frame type (tibble vs data.frame)
  result <- Reduce(
    f = function(acc, scrambled_df) {
      acc[names(scrambled_df)] <- scrambled_df
      acc
    },
    x = scrambled_dfs,
    init = result
  )

  # Restore original column order (same as original implementation)
  result[names(data)]
}
