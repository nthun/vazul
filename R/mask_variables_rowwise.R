#' Mask categorical labels across multiple column sets rowwise in a data frame
#'
#' For each row, independently mask labels within each group of selected columns.
#' This function uses mask_labels_rowwise() for each column set.
#'
#' @param data A data frame.
#' @param ... <tidy-select> One or more column sets. Each can be:
#'   \itemize{
#'     \item A tidyselect expression (e.g., \code{starts_with("treat_")})
#'     \item A character vector of column names (e.g., \code{c("treat_1", "treat_2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#' @param prefix character string to use as prefix for masked labels.
#'   Default is "masked_group_"
#' @return A data frame with labels masked rowwise within each selected column set.
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
#' set.seed(123)
#' df |> mask_variables_rowwise(starts_with("treat_"))
#' df |> mask_variables_rowwise(c("treat_1", "treat_2"))
#' df |> mask_variables_rowwise(
#'   starts_with("treat_"),
#'   c("condition_a", "condition_b")
#' )
#' @export
mask_variables_rowwise <- function(data, ..., prefix = "masked_group_") {
  stopifnot(is.data.frame(data))
  
  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)
  
  # If no sets provided, return data unchanged
  if (length(column_sets) == 0) {
    warning("No column sets provided. Returning data unchanged.", 
            call. = FALSE)
    return(data)
  }
  
  # Apply mask_labels_rowwise to each column set
  # This is much simpler - just map the function across all sets
  masked_dfs <- lapply(column_sets, function(set_quo) {
    # Get column names for this set
    tryCatch({
      col_indices <- tidyselect::eval_select(set_quo, data)
      col_names <- names(data)[col_indices]
      
      if (length(col_names) == 0) {
        return(NULL)
      }
      
      # Apply mask_labels_rowwise to this set and return only the masked columns
      mask_labels_rowwise(data, dplyr::all_of(col_names), prefix = prefix)[col_names]
    }, error = function(e) {
      warning("Failed to evaluate column set: ", conditionMessage(e), 
              call. = FALSE)
      return(NULL)
    })
  })
  
  # Filter out NULL results
  masked_dfs <- Filter(Negate(is.null), masked_dfs)
  
  # If nothing was masked, return original
  if (length(masked_dfs) == 0) {
    return(data)
  }
  
  # Start with original data to preserve class
  result <- data
  
  # Use functional approach to assign masked columns back
  result <- Reduce(
    f = function(acc, masked_df) {
      acc[names(masked_df)] <- masked_df
      acc
    },
    x = masked_dfs,
    init = result
  )
  
  # Restore original column order
  result[names(data)]
}