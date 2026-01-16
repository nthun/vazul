#' Mask categorical labels across multiple columns rowwise in a data frame
#' 
#' For each row, independently mask labels within the selected columns.
#' All selected columns are combined into a single set and processed together.
#' To mask different variable groups separately, call the function multiple times.
#' @keywords mask
#' @param data A data frame.
#' @param ... <tidy-select> Columns to mask. All arguments are combined into
#'   a single set. Each can be:
#'   \itemize{
#'     \item Bare column names (e.g., \code{var1, var2})
#'     \item A tidyselect expression (e.g., \code{starts_with("treat_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'   }
#' @param prefix character string to use as prefix for masked labels.
#'   Default is "masked_group_"
#' @return A data frame with labels masked rowwise within the selected columns.
#' 
#' @seealso \code{\link{mask_labels}} for masking a single vector, 
#' \code{\link{mask_variables}} for masking multiple variables, and 
#' \code{\link{mask_names}} for masking variable names.
#' 
#' @examples
#' df <- data.frame(
#'   treat_1 = c("control", "treatment", "placebo"),
#'   treat_2 = c("treatment", "placebo", "control"),
#'   treat_3 = c("placebo", "control", "treatment"),
#'   condition_a = c("A", "B", "A"),
#'   condition_b = c("B", "A", "B"),
#'   id = 1:3
#' )
#'
#' set.seed(1037)
#' # Mask one set of variables
#' library(dplyr)
#' df |> mask_variables_rowwise(starts_with("treat_"))
#' 
#' # Using character vectors
#' df |> mask_variables_rowwise(c("treat_1", "treat_2", "treat_3"))
#' 
#' # Mask multiple sets separately
#' df |>
#'   mask_variables_rowwise(starts_with("treat_")) |>
#'   mask_variables_rowwise(c("condition_a", "condition_b"))
#' 
#' # Example with custom prefix
#' df |> mask_variables_rowwise(starts_with("treat_"), prefix = "group_")
#' 
#' @export
mask_variables_rowwise <- function(data, ..., prefix = "masked_group_") {
  validate_data_frame(data)
  validate_data_frame_not_empty(data)
  validate_prefix(prefix)

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # Resolve all column sets to column names (combined sets)
  all_col_names <- resolve_all_column_sets(column_sets, data)

  if (!validate_column_selection_not_empty(all_col_names)) {
    return(data)
  }

  # Check that all selected columns are character or factor
  validate_columns_categorical(data, all_col_names)

  # Get all unique values across all selected columns to create consistent mapping
  all_values <- collect_unique_values(data, all_col_names)

  # Create a mapping using all unique values
  mapping <- create_mapping(all_values, prefix = prefix)

  if (!validate_mapping_not_empty(mapping)) {
    return(data)
  }

  # Copy data
  result <- data

  # Apply mapping to each selected column
  result[all_col_names] <- lapply(result[all_col_names], function(col) {
    apply_mapping(col, mapping)
  })

  return(result)
}
