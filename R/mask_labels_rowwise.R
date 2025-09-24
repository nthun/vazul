#' Mask categorical labels rowwise in a data frame
#'
#' For each row, mask the categorical labels across the selected columns using
#' consistent masked labels. This function applies mask_labels() to the unique
#' values found across all selected columns to ensure consistency.
#'
#' @param data a data frame
#' @param cols <tidy-select> Columns to mask rowwise. Supports helpers like 
#'   \code{starts_with()}, \code{contains()}, \code{where()}, etc.
#' @param prefix character string to use as prefix for masked labels.
#'   Default is "masked_group_"
#'
#' @return A data frame with labels masked rowwise across the selected columns.
#'
#' @examples
#' df <- data.frame(
#'   treat_1 = c("control", "treatment", "placebo"),
#'   treat_2 = c("treatment", "placebo", "control"),
#'   treat_3 = c("placebo", "control", "treatment"),
#'   id = 1:3
#' )
#'
#' set.seed(123)
#' df |> mask_labels_rowwise(starts_with("treat_"))
#' df |> mask_labels_rowwise(c("treat_1", "treat_2"))
#'
#' @export
mask_labels_rowwise <- function(data, cols, prefix = "masked_group_") {
  
  stopifnot(is.data.frame(data))
  
  # Capture as quosure and evaluate column selection
  col_indices <- tidyselect::eval_select(rlang::enquo(cols), data)
  col_names <- names(data)[col_indices]
  
  # Validate at least one column selected
  if (length(col_names) == 0) {
    stop("No columns selected. Please provide valid column names or ", 
         "selection helpers.", call. = FALSE)
  }
  
  # Check that all selected columns are character or factor
  col_types <- vapply(data[col_names], function(x) {
    is.character(x) || is.factor(x)
  }, logical(1))
  
  if (!all(col_types)) {
    invalid_cols <- col_names[!col_types]
    stop("All selected columns must be character or factor vectors. ",
         "Invalid columns: ", paste(invalid_cols, collapse = ", "), 
         call. = FALSE)
  }
  
  # Get all unique values across all selected columns to create consistent mapping
  all_values <- unique(unlist(lapply(data[col_names], function(x) {
    if (is.factor(x)) as.character(x) else x
  }), use.names = FALSE))
  all_values <- all_values[!is.na(all_values)]  # Remove NAs
  
  if (length(all_values) == 0) {
    warning("No non-NA values found in selected columns.", call. = FALSE)
    return(data)
  }
  
  # Create the mapping using mask_labels on all unique values
  mapped_labels <- mask_labels(all_values, prefix = prefix)
  mapping <- stats::setNames(mapped_labels, all_values)
  
  # Copy data
  result <- data
  
  # Apply mapping to each selected column using base R
  result[col_names] <- lapply(data[col_names], function(col) {
    if (is.factor(col)) {
      char_values <- as.character(col)
      masked_values <- ifelse(is.na(char_values), NA_character_, 
                              mapping[char_values])
      # Get all possible masked labels for factor levels
      all_masked <- unique(mapping)
      factor(masked_values, levels = all_masked)
    } else {
      ifelse(is.na(col), NA_character_, mapping[col])
    }
  })
  
  result
}