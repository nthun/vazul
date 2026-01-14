#' Mask variable names with anonymous labels
#'
#' Assigns new masked names to selected variables in a data frame.
#' All selected variables are combined into a single set and renamed with
#' a common prefix. To mask different variable groups with different prefixes,
#' call the function separately for each group.
#' @keywords functions
#' @param data A data frame.
#' @param ... <tidy-select> Columns to mask. All arguments are combined into
#'   a single set. Each can be:
#'   \itemize{
#'     \item Bare column names (e.g., \code{var1, var2})
#'     \item A tidyselect expression (e.g., \code{starts_with("treatment_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'   }
#' @param prefix character string to use as prefix for masked names.
#'   This becomes the base prefix, with numeric suffixes appended (e.g., 
#'   \code{prefix = "treatment_"} produces "treatment_01", "treatment_02", etc.).
#'   The prefix is used as-is, so include a separator (e.g., underscore) if desired.
#'
#' @return A data frame with the specified variables renamed to masked names.
#'
#' @examples
#' df <- data.frame(
#'   treat_1 = c(1, 2, 3),
#'   treat_2 = c(4, 5, 6),
#'   outcome_a = c(7, 8, 9),
#'   outcome_b = c(10, 11, 12),
#'   id = 1:3
#' )
#'
#' # Mask one set of variables
#' library(dplyr)
#' mask_names(df, starts_with("treat_"), prefix = "treatment_")
#'
#' # Using character vectors
#' mask_names(df, c("treat_1", "treat_2"), prefix = "treatment_")
#'
#' # Mask multiple sets separately
#' df |>
#'   mask_names(starts_with("treat_"), prefix = "treatment_") |>
#'   mask_names(starts_with("outcome_"), prefix = "outcome_")
#'
#' # Example with the 'williams' dataset
#' data(williams)
#' set.seed(42)
#'
#' williams |>
#'   mask_names(starts_with("SexUnres"), prefix = "A_") |>
#'   mask_names(starts_with("Impul"), prefix = "B_") |>
#'   colnames()
#'
#' @export
mask_names <- function(data, ..., prefix) {
  validate_data_frame(data)
  validate_data_frame_not_empty(data)

  # Validate prefix parameter
  if (missing(prefix)) {
    stop("Parameter 'prefix' is required. Please provide a character string ",
         "to use as the prefix for masked names.", call. = FALSE)
  }
  validate_prefix(prefix)

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # Resolve all column sets to column names (combined sets)
  all_col_names <- resolve_all_column_sets(column_sets, data)

  if (!validate_column_selection_not_empty(all_col_names)) {
    return(data)
  }

  # Create masked names using mask_labels() with prefix
  # Use prefix directly (no normalization)
  masked_names <- mask_labels(all_col_names, prefix = prefix)

  # Check for name collisions early (before creating mapping)
  existing_names <- setdiff(names(data), all_col_names)
  name_collisions <- intersect(masked_names, existing_names)

  if (length(name_collisions) > 0) {
    stop("Name collision detected. The following masked names already ",
         "exist in the data: ", paste(name_collisions, collapse = ", "),
         ". Please use a different 'prefix'.", call. = FALSE)
  }

  # Warn if masked names share prefix with existing columns (potential confusion)
  existing_with_same_prefix <- existing_names[startsWith(existing_names, prefix)]
  if (length(existing_with_same_prefix) > 0) {
    warning("Masked names use prefix '", prefix, "' which matches existing ",
            "column(s): ", paste(existing_with_same_prefix, collapse = ", "),
            ". This may cause confusion. Consider using a different 'prefix'.",
            call. = FALSE)
  }

  # Create mapping from original to masked names
  final_mapping <- stats::setNames(masked_names, all_col_names)

  # Apply the name changes to the data frame
  result <- data
  names(result)[match(names(final_mapping), names(result))] <- final_mapping

  return(result)
}
