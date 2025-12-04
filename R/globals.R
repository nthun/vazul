# Register global variables used in dplyr/tidyeval contexts
# This suppresses R CMD check notes about undefined variables

utils::globalVariables(c(".row_id"))
utils::globalVariables(c(".scrambled_rows"))

# Ensure 'utils' is visibly used (avoids false "not imported" NOTE)
# Use a condition that is not statically FALSE
utils::capture.output(NULL)


# ──────────────────────────────────────────────────────────────────────────────
# Generic input validation helper functions
# ──────────────────────────────────────────────────────────────────────────────

#' Validate that input is a data frame
#'
#' Internal helper function for validating data frame input.
#'
#' @param data The input to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame. Received object of class: ",
         paste(class(data), collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate that a data frame is not empty
#'
#' Internal helper function for validating that a data frame has rows.
#'
#' @param data The data frame to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_data_frame_not_empty <- function(data) {
  if (nrow(data) == 0) {
    stop("Input 'data' cannot be an empty data frame.", call. = FALSE)
  }
  invisible(NULL)
}

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
