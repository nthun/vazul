#' Resolve a single column set from a quosure
#'
#' Internal helper function that resolves column names from a quosure.
#' Handles both character vectors and tidyselect expressions.
#'
#' @param set_quo A quosure captured from ... arguments.
#' @param data The data frame to resolve columns from.
#' @return A character vector of column names, or character(0) if resolution
#'   fails.
#' @keywords internal
#' @noRd
resolve_column_set <- function(set_quo, data) {
  # Helper to validate and return character column selection
  validate_char_columns <- function(set) {
    missing <- setdiff(set, names(data))
    if (length(missing) > 0) {
      stop("Error in column selection: Can't subset columns that ",
           "don't exist. Column `",
           paste(missing, collapse = "`, `"),
           "` doesn't exist.", call. = FALSE)
    }
    set
  }

  # Helper to use tidyselect and return column names
  select_with_tidyselect <- function(quo) {
    selected <- tryCatch(
      tidyselect::eval_select(quo, data),
      error = function(e) {
        stop("Error in column selection: ", conditionMessage(e),
             call. = FALSE)
      }
    )
    if (length(selected) == 0) return(character(0))
    names(data)[selected]
  }

  # If quosure is a bare symbol (column name), use tidyselect directly
  # This prevents evaluating the symbol to its column values
  if (rlang::quo_is_symbol(set_quo)) {
    return(select_with_tidyselect(set_quo))
  }

  # If it's a call (like starts_with() or c()), try evaluating first
  # to handle character vectors and numeric indices
  result <- tryCatch(
    expr = {
      set <- rlang::eval_tidy(set_quo, data = data)
      if (is.character(set)) {
        validate_char_columns(set)
      } else if (is.numeric(set)) {
        select_with_tidyselect(set_quo)
      } else {
        NULL
      }
    },
    error = function(e) {
      if (grepl("Error in column selection", conditionMessage(e))) {
        stop(conditionMessage(e), call. = FALSE)
      }
      NULL
    }
  )

  if (!is.null(result)) return(result)

  # If evaluation failed, treat as tidyselect expression
  if (rlang::quo_is_call(set_quo)) {
    return(select_with_tidyselect(set_quo))
  }

  warning("Each column set must be a character vector or ",
          "tidyselect expression.", call. = FALSE)
  character(0)
}

#' Resolve all column sets from ... arguments
#'
#' Internal helper function that resolves column names from multiple quosures.
#'
#' @param column_sets A list of quosures captured from ... arguments.
#' @param data The data frame to resolve columns from.
#' @return A character vector of unique column names.
#' @keywords internal
#' @noRd
resolve_all_column_sets <- function(column_sets, data) {
  all_col_names <- unlist(lapply(column_sets, resolve_column_set, data = data),
                          use.names = FALSE)
  unique(all_col_names)
}

#' Resolve group columns from .groups parameter
#'
#' Internal helper function that resolves group column names from a quosure.
#' Provides consistent error handling and validation for grouping columns.
#'
#' @param groups_quo A quosure captured from the .groups parameter.
#' @param data The data frame to resolve columns from.
#' @param col_names Character vector of column names being scrambled/masked.
#'   Used to check for overlap with grouping columns.
#' @return A character vector of group column names, or NULL if no groups.
#' @keywords internal
#' @noRd
resolve_group_columns <- function(groups_quo, data, col_names = NULL) {
  # If .groups is NULL, return NULL early
  if (rlang::quo_is_null(groups_quo)) {
    return(NULL)
  }

  # Unwrap rlang::expr(...) / expr(...) so users can pass `.groups = rlang::expr(starts_with("x"))`
  # and have it behave like `.groups = starts_with("x")`.
  groups_expr <- rlang::get_expr(groups_quo)
  if (rlang::is_call(groups_expr) && identical(rlang::call_name(groups_expr), "expr")) {
    groups_quo <- rlang::new_quosure(groups_expr[[2]], env = rlang::get_env(groups_quo))
  }

  # Evaluate the tidyselect expression with error handling
  group_indices <- tryCatch(
    tidyselect::eval_select(groups_quo, data),
    error = function(e) {
      stop("Error in .groups parameter: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  
  # Check if selection is empty
  if (length(group_indices) == 0) {
    stop("Error in .groups parameter: No columns selected for grouping.",
         call. = FALSE)
  }
  
  # Get group column names
  group_cols <- names(data)[group_indices]
  
  # Check for overlap with columns being processed (if provided)
  if (!is.null(col_names)) {
    overlap <- intersect(group_cols, col_names)
    if (length(overlap) > 0) {
      stop("Grouping columns cannot overlap with columns being processed. ",
           "The following columns are in both: ",
           paste(overlap, collapse = ", "), ".", call. = FALSE)
    }
  }
  
  return(group_cols)
}