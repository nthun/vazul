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

#' Validate that input is a vector
#'
#' Internal helper function for validating vector input.
#' Checks that input is not NULL, is atomic or list, is not a matrix or
#' data frame, and is not empty.
#'
#' @param x The input to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_vector <- function(x) {
  if (is.null(x)) {
    stop("Input 'x' cannot be NULL. Please provide a vector.", call. = FALSE)
  }

  if (!is.atomic(x) && !is.list(x)) {
    stop("Input 'x' must be an atomic vector or list. ",
         "Received object of class: ",
         paste(class(x), collapse = ", "), ".", call. = FALSE)
  }

  if (is.matrix(x) || is.data.frame(x)) {
    stop("Input 'x' must be a 1-dimensional vector. Received object of class: ",
         paste(class(x), collapse = ", "), ".", call. = FALSE)
  }

  if (length(x) == 0) {
    stop("Input 'x' cannot be an empty vector. Please provide a vector ",
         "with at least one element.", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate that input is a categorical vector
#'
#' Internal helper function for validating that a vector is categorical
#' (character or factor). Must be called after validate_vector().
#'
#' @param x The vector to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_vector_categorical <- function(x) {
  if (!is.character(x) && !is.factor(x)) {
    stop("Input 'x' must be a character or factor vector. ",
         "Received object of class: ",
         paste(class(x), collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate prefix parameter
#'
#' Internal helper function for validating prefix parameter.
#' Checks that prefix is not NULL, is character, and has length 1.
#'
#' @param prefix The prefix to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_prefix <- function(prefix) {
  if (is.null(prefix)) {
    stop("Parameter 'prefix' cannot be NULL. Please provide a character ",
         "string.", call. = FALSE)
  }

  if (!is.character(prefix) || length(prefix) != 1) {
    stop("Parameter 'prefix' must be a single character string.",
         call. = FALSE)
  }

  invisible(NULL)
}

#' Validate logical parameter
#'
#' Internal helper function for validating logical parameters.
#' Checks that parameter is not NULL, is logical, and has length 1.
#'
#' @param param The logical parameter to validate.
#' @param param_name The name of the parameter for error messages.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_logical_parameter <- function(param, param_name) {
  if (is.null(param)) {
    stop("Parameter '", param_name, "' cannot be NULL. ",
         "Please provide a logical value.", call. = FALSE)
  }

  if (!is.logical(param) || length(param) != 1) {
    stop("Parameter '", param_name, "' must be a single logical value ",
         "(TRUE or FALSE).", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate that column selection is not empty
#'
#' Internal helper function for validating that column selection resulted in
#' at least one column. Emits a warning if selection is empty.
#'
#' @param selected_cols Character vector of resolved column names.
#' @return Logical. `TRUE` if selection is valid (has columns), `FALSE` if empty
#'   (after emitting a warning).
#' @keywords internal
#' @noRd
validate_column_selection_not_empty <- function(selected_cols) {
  if (is.null(selected_cols) || length(selected_cols) == 0) {
    warning("No columns selected. Returning original data unchanged.",
            call. = FALSE)
    return(FALSE)
  }
  TRUE
}

#' Validate that selected columns are categorical (character or factor)
#'
#' Internal helper function for validating that all selected columns in a data
#' frame are categorical (character or factor vectors).
#'
#' @param data The data frame to validate columns from.
#' @param col_names Character vector of column names to validate.
#' @return NULL (invisibly). Throws an error if validation fails.
#' @keywords internal
#' @noRd
validate_columns_categorical <- function(data, col_names) {
  is_categorical <- vapply(data[col_names], function(x) {
    is.character(x) || is.factor(x)
  }, logical(1))

  non_categorical_cols <- col_names[!is_categorical]

  if (length(non_categorical_cols) > 0) {
    stop(
      "The following selected columns are not character or factor: ",
      paste(non_categorical_cols, collapse = ", "),
      ". Only character and factor columns can be masked.", call. = FALSE
    )
  }
  
  invisible(NULL)
}