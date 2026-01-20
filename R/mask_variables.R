#' Mask categorical variables with random labels in a data frame
#'
#' Applies masked labels to multiple categorical variables in a data frame using
#' the \code{mask_labels()} function. Each variable gets independent random
#' masked labels by default, or can optionally use the same masked labels
#' across all selected variables.
#' @keywords mask
#' @param data a data frame
#' @param ... Columns to mask using tidyselect semantics. Each can be:
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
#' @seealso \code{\link{mask_labels}} for masking a single vector,
#' \code{\link{mask_variables_rowwise}} for rowwise masking, and
#' \code{\link{mask_names}} for masking variable names.
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
  validate_logical_parameter(across_variables, "across_variables")

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # Resolve all column sets to column names
  all_col_names <- resolve_all_column_sets(column_sets, data)

  if (!validate_column_selection_not_empty(all_col_names)) {
    return(data)
  }

  # Validate that all selected columns are categorical
  validate_columns_categorical(data, all_col_names)

  # Apply masking
  result <- data

  if (across_variables) {
    # For across_variables masking, create shared mapping using mask_labels
    # First, collect all unique values across all selected categorical columns
    all_values <- collect_unique_values(data, all_col_names)

    # Create a shared mapping across all selected columns
    mapping <- create_mapping(all_values, prefix = "masked_group_")

    if (!validate_mapping_not_empty(mapping)) {
      return(data)
    }

    # Apply shared mapping to each column
    result[all_col_names] <- lapply(result[all_col_names], function(x) {
      apply_mapping(x, mapping)
    })

  } else {
    # Independent masking for each column using column name as prefix
    # First, check for all-NA columns to avoid multiple warnings
    all_na_cols <- all_col_names[
      vapply(result[all_col_names], function(x) all(is.na(x)), logical(1))
    ]

    result[all_col_names] <- lapply(all_col_names, function(col_name) {
      x <- result[[col_name]]

      if (col_name %in% all_na_cols) {
        return(x)
      }

      mask_labels(x, prefix = paste0(col_name, "_group_"))
    })

    # Emit a single consolidated warning for all-NA columns
    if (length(all_na_cols) > 0) {
      warning(
        "The following columns contain only NA values and were left unchanged: ",
        paste(all_na_cols, collapse = ", "), ".", call. = FALSE
      )
    }
  }

  return(result)
}
