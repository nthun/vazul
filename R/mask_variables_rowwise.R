#' Mask categorical labels rowwise across multiple column sets
#'
#' For each row, independently mask labels across each group of selected 
#' columns. The masked labels are drawn from all unique values across all
#' specified columns and all rows, ensuring consistent masking within each
#' column set.
#'
#' @param data A data frame.
#' @param ... <tidy-select> One or more column sets. Each can be:
#'   \itemize{
#'     \item A tidyselect expression (e.g., 
#'       \code{starts_with("condition_")})
#'     \item A character vector of column names (e.g., 
#'       \code{c("treat_1", "treat_2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#' @param prefix character string to use as prefix for masked labels.
#'   Default is "masked_group_"
#' @return A data frame with labels masked rowwise within each selected 
#'   column set.
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

  # Helper to handle one set
  mask_one_set <- function(set_quo) {
    # Try evaluating as character vector first
    try_char <- tryCatch(
      expr = {
        set <- rlang::eval_tidy(set_quo, data = data)
        if (is.character(set)) {
          missing <- setdiff(set, names(data))
          if (length(missing) > 0) {
            warning("Some column names not found: ", 
                    paste(missing, collapse = ", "), call. = FALSE)
            return(NULL)
          }
          return(mask_labels_rowwise_simple(data, set, prefix = prefix))
        } else {
          # Not character — fall through to tidyselect
          NULL
        }
      },
      error = function(e) {
        # Not a character vector — fall through to tidyselect
        NULL
      }
    )

    if (!is.null(try_char)) {
      return(try_char)
    }

    # If not character, treat as tidyselect expression
    if (rlang::quo_is_symbol(set_quo) || rlang::quo_is_call(set_quo)) {
      selected <- tryCatch(
        tidyselect::eval_select(set_quo, data),
        error = function(e) {
          warning("Failed to evaluate column set: ", conditionMessage(e), 
                  call. = FALSE)
          return(NULL)
        }
      )
      if (length(selected) == 0) return(NULL)
      col_names <- names(data)[selected]
      mask_labels_rowwise_simple(data, col_names, prefix = prefix)
    } else {
      warning("Each column set must be a character vector or tidyselect ", 
              "expression.", call. = FALSE)
      return(NULL)
    }
  }

  # Apply to each set
  masked_dfs <- lapply(column_sets, mask_one_set)
  masked_dfs <- Filter(Negate(is.null), masked_dfs)

  # If nothing was masked, return original
  if (length(masked_dfs) == 0) {
    return(data)
  }

  # Start with original data to preserve class
  result <- data
  
  # Use functional approach to assign masked columns back
  # This preserves the original data frame type (tibble vs data.frame)
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

#' Helper function to mask labels across selected columns (simple version)
#'
#' @param data A data frame
#' @param col_names Character vector of column names
#' @param prefix Prefix for masked labels
#' @return Data frame with masked columns only
#' @keywords internal
mask_labels_rowwise_simple <- function(data, col_names, 
                                       prefix = "masked_group_") {
  stopifnot(is.data.frame(data))

  # Validate at least one column selected
  if (length(col_names) == 0) {
    stop("No columns selected. Please provide valid column names or ", 
         "selection helpers.", call. = FALSE)
  }

  # Check that all selected columns are character or factor
  for (col_name in col_names) {
    if (!is.character(data[[col_name]]) && !is.factor(data[[col_name]])) {
      stop("All selected columns must be character or factor vectors. ", 
           "Column '", col_name, "' is of type: ", typeof(data[[col_name]]), 
           ".", call. = FALSE)
    }
  }

  # Get all unique values across ALL selected columns and ALL rows
  # This is the key requirement: labels from all rows, not just current row
  all_values <- unique(unlist(lapply(data[col_names], function(x) {
    if (is.factor(x)) as.character(x) else x
  })))
  all_values <- all_values[!is.na(all_values)]  # Remove NAs from universe
  
  # If no values found, return original columns
  if (length(all_values) == 0) {
    warning("No non-NA values found in selected columns.", call. = FALSE)
    return(data[col_names])
  }

  # Create a single mapping for all values in this column set
  # This ensures consistency across rows and columns
  n_unique <- length(all_values)
  padding_width <- max(2, nchar(as.character(n_unique)))
  pattern <- paste0("%0", padding_width, "d")
  masked_labels <- paste0(prefix, sprintf(pattern, seq_len(n_unique)))
  random_assignment <- sample(masked_labels, n_unique, replace = FALSE)
  mapping <- stats::setNames(random_assignment, all_values)

  # Copy data
  result <- data[col_names]

  # Apply mapping to each selected column
  for (col_name in col_names) {
    original_col <- data[[col_name]]
    
    # Convert to character for mapping
    if (is.factor(original_col)) {
      char_values <- as.character(original_col)
    } else {
      char_values <- original_col
    }
    
    # Apply mapping, keeping NAs as NAs
    masked_values <- ifelse(is.na(char_values), NA_character_, 
                            mapping[char_values])
    
    # Preserve factor structure if original was a factor
    if (is.factor(original_col)) {
      # For factors, we need to determine the appropriate levels
      # Use all possible masked labels for consistency
      result[[col_name]] <- factor(masked_values, levels = masked_labels)
    } else {
      result[[col_name]] <- masked_values
    }
  }

  result
}