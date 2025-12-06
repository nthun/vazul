#' Mask categorical variables with random labels in a data frame
#'
#' Applies masked labels to multiple categorical variables in a data frame using
#' the \code{mask_labels()} function. Each variable gets independent random
#' masked labels by default, or can optionally use the same masked labels
#' across all selected variables.
#' @keywords functions
#' @param data a data frame
#' @param ... <tidy-select> Columns to mask. Each can be:
#'   \itemize{
#'     \item Bare column names (e.g., \code{var1, var2})
#'     \item A tidyselect expression (e.g., \code{starts_with("treat_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#'   Only character and factor columns will be processed.
#' @param across_variables logical. If \code{TRUE}, all selected variables will
#'   use the same set of masked labels. If \code{FALSE} (default), each variable
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
#' # Independent masking for each variable (default - uses column names as
#' # prefixes)
#' # Using bare names
#' mask_variables(df, treatment, outcome)
#' # Or using character vector
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
#' # Using bare names (recommended for interactive use)
#' williams_masked <- mask_variables(williams, subject, ecology)
#' head(williams_masked[c("subject", "ecology")])
#'
#' @export
mask_variables <- function(data, ..., across_variables = FALSE) {
  validate_data_frame(data)
  validate_data_frame_not_empty(data)

  # Validate across_variables parameter
  if (is.null(across_variables)) {
    stop("Parameter 'across_variables' cannot be NULL. ",
         "Please provide a logical value.", call. = FALSE)
  }

  if (!is.logical(across_variables) || length(across_variables) != 1) {
    stop("Parameter 'across_variables' must be a single logical value ",
         "(TRUE or FALSE).", call. = FALSE)
  }

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # If no sets provided, return data unchanged
  if (length(column_sets) == 0) {
    warning("No columns selected. Returning original data unchanged.",
            call. = FALSE)
    return(data)
  }

  # Resolve all column sets to column names
  all_col_names <- resolve_all_column_sets(column_sets, data)

  if (length(all_col_names) == 0) {
    warning("No columns selected. Returning original data unchanged.",
            call. = FALSE)
    return(data)
  }

  # Validate that all selected columns are categorical
  is_categorical <- vapply(data[all_col_names], function(x) {
    is.character(x) || is.factor(x)
  }, logical(1))

  non_categorical_cols <- all_col_names[!is_categorical]

  # Error if non-categorical columns are selected
  if (length(non_categorical_cols) > 0) {
    stop(
      "The following selected columns are not character or factor: ",
      paste(non_categorical_cols, collapse = ", "),
      ". Only character and factor columns can be masked.", call. = FALSE
    )
  }

  categorical_cols <- all_col_names

  # Apply masking
  result <- data

  if (across_variables) {
    # For across_variables masking, create shared mapping using mask_labels
    # First, collect all unique values across all selected categorical columns
    all_values <- unique(unlist(lapply(result[categorical_cols], function(x) {
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
    result[categorical_cols] <- lapply(result[categorical_cols], function(x) {
      if (all(is.na(x))) {
        return(x)  # Return unchanged if all NA
      }

      # Apply mapping
      masked <- shared_mapping[as.character(x)]

      # Preserve factor structure if input was a factor
      if (is.factor(x)) {
        masked <- factor(masked, levels = unique(masked_values))
      }

      return(masked)
    })

  } else {
    # Independent masking for each column using column name as prefix
    result[categorical_cols] <- lapply(categorical_cols, function(col_name) {
      x <- result[[col_name]]
      if (all(is.na(x))) {
        return(x)  # Return unchanged if all NA
      }

      # Use column name as prefix for independent masking
      col_prefix <- paste0(col_name, "_group_")

      # Apply mask_labels to each column independently with column-specific
      # prefix
      tryCatch({
        mask_labels(x, prefix = col_prefix)
      }, error = function(e) {
        warning("Failed to mask column: ", conditionMessage(e),
                call. = FALSE)
        return(x)  # Return original column if masking fails
      })
    })
  }

  return(result)
}
