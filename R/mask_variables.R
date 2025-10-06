#' Mask categorical variables with random labels in a data frame
#'
#' Applies masked labels to multiple categorical variables in a data frame using
#' the \code{mask_labels()} function. Each variable gets independent random
#' masked labels by default, or can optionally use the same masked labels
#' across all selected variables.
#' @keywords functions
#' @param data a data frame
#' @param ... <tidy-select> Columns to mask. Accepts column names, positions,
#'   or tidyselect helpers like \code{starts_with()}, \code{contains()},
#'   \code{where()}, etc. Only character and factor columns will be
#'   processed.
#' @param across_variables logical. If \code{TRUE}, all selected variables will use
#'   the same set of masked labels. If \code{FALSE} (default), each variable
#'   gets its own independent set of masked labels using the column name as
#'   prefix.
#'
#' @return A data frame with the specified categorical columns masked.
#'   Only character and factor columns can be processed.
#'
#' @examples
#'
#' # Create example data
#' df <- data.frame(
#'   treatment = c("control", "intervention", "control"),
#'   outcome = c("success", "failure", "success"),
#'   score = c(1, 2, 3)  # numeric, won't be masked
#' )
#'
#' set.seed(123)
#' # Independent masking for each variable (default - uses column names as prefixes)
#' mask_variables(df, c("treatment", "outcome"))
#'
#' set.seed(456)
#' # Shared masking across variables
#' mask_variables(df, c("treatment", "outcome"), across_variables = TRUE)
#'
#' # Using tidyselect helpers
#' mask_variables(df, where(is.character))
#'
#' # Example with multiple categorical columns
#' df2 <- data.frame(
#'   group = c("A", "B", "A", "B"),
#'   condition = c("ctrl", "test", "ctrl", "test")
#' )
#' set.seed(123)
#' result <- mask_variables(df2, c("group", "condition"))
#' print(result)
#'
#' # Example with williams dataset (multiple categorical columns)
#' data(williams)
#' set.seed(456)
#' williams_masked <- mask_variables(williams, c("subject", "ecology"))
#' head(williams_masked[c("subject", "ecology")])
#'
#' @export
mask_variables <- function(data, ..., across_variables = FALSE) {
  # Input validation
  if (is.null(data)) {
    stop("Input 'data' cannot be NULL. Please provide a data frame.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame. ",
         "Received object of class: ",
         paste(class(data), collapse = ", "), ".", call. = FALSE)
  }

  if (nrow(data) == 0) {
    stop("Input 'data' cannot be an empty data frame.", call. = FALSE)
  }

  # Validate across_variables parameter
  if (is.null(across_variables)) {
    stop("Parameter 'across_variables' cannot be NULL. ",
         "Please provide a logical value.", call. = FALSE)
  }

  if (!is.logical(across_variables) || length(across_variables) != 1) {
    stop("Parameter 'across_variables' must be a single logical value ",
         "(TRUE or FALSE).", call. = FALSE)
  }

  # Handle column selection using tidyselect
  cols_quo <- rlang::enquos(...)
  if (length(cols_quo) == 0) {
    stop("No columns specified for masking.", call. = FALSE)
  }
  # Combine all arguments into a single selection
  col_indices <- tryCatch({
    tidyselect::eval_select(rlang::expr(c(!!!cols_quo)), data)
  }, error = function(e) {
    stop("Error in column selection: ", conditionMessage(e), call. = FALSE)
  })

  if (length(col_indices) == 0) {
    warning("No columns selected. Returning original data unchanged.", call. = FALSE)
    return(data)
  }

  # Validate that all selected columns are categorical
  selected_cols <- names(data)[col_indices]
  is_categorical <- vapply(data[selected_cols], function(x) {
    is.character(x) || is.factor(x)
  }, logical(1))

  non_categorical_cols <- selected_cols[!is_categorical]

  # Error if non-categorical columns are selected
  if (length(non_categorical_cols) > 0) {
    stop(
      "The following selected columns are not character or factor: ",
      paste(non_categorical_cols, collapse = ", "),
      ". Only character and factor columns can be masked.", call. = FALSE
    )
  }

  categorical_cols <- selected_cols

  # Apply masking
  if (across_variables) {
    # For across_variables masking, create shared mapping using mask_labels
    # First, collect all unique values across all selected categorical columns
    all_values <- unique(unlist(lapply(data[categorical_cols], function(x) {
      if (is.factor(x)) {
        as.character(x)
      } else {
        x
      }
    }), use.names = FALSE))

    # Remove NAs for mapping creation
    all_values_no_na <- all_values[!is.na(all_values)]

    if (length(all_values_no_na) == 0) {
      warning(
        "All values in selected categorical columns are NA. ",
        "Returning original data unchanged.", call. = FALSE
      )
      return(data)
    }

    # Use mask_labels to create the shared mapping
    masked_values <- mask_labels(all_values_no_na, prefix = "masked_group_")
    shared_mapping <- stats::setNames(masked_values, all_values_no_na)

    # Apply shared mapping to each column
    data[categorical_cols] <- lapply(data[categorical_cols], function(x) {
      if (all(is.na(x))) {
        return(x)  # Return unchanged if all NA
      }

      # Apply mapping
      result <- shared_mapping[as.character(x)]

      # Preserve factor structure if input was a factor
      if (is.factor(x)) {
        # For factors with shared labels, use all possible masked labels as
        # levels
        result <- factor(result, levels = unique(masked_values))
      }

      return(result)
    })

  } else {
    # Independent masking for each column
    # Use column name as part of prefix to avoid label collisions
    data[categorical_cols] <- lapply(names(data[categorical_cols]), function(col_name) {
      x <- data[[col_name]]
      if (all(is.na(x))) {
        return(x)  # Return unchanged if all NA
      }

      # Use column name as prefix for independent masking
      col_prefix <- paste0(col_name, "_group_")

      # Apply mask_labels to each column independently with column-specific prefix
      tryCatch({
        mask_labels(x, prefix = col_prefix)
      }, error = function(e) {
        warning("Failed to mask column: ", conditionMessage(e),
                call. = FALSE)
        return(x)  # Return original column if masking fails
      })
    })
    names(data[categorical_cols]) <- categorical_cols
  }

  return(data)
}
