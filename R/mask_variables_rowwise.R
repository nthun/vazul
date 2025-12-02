#' Mask categorical labels across multiple column sets rowwise in a data frame
#'
#' For each row, independently mask labels within each group of selected columns.
#' @keywords functions
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
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame. Received object of class: ",
         paste(class(data), collapse = ", "), ".", call. = FALSE)
  }

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # If no sets provided, return data unchanged
  if (length(column_sets) == 0) {
    warning("No column sets provided. Returning data unchanged.",
            call. = FALSE)
    return(data)
  }

  # Internal helper: mask labels rowwise for a single set of columns
  mask_labels_rowwise_internal <- function(df, col_names, mask_prefix) {
    # Validate at least one column selected
    if (length(col_names) == 0) {
      stop("No columns selected. Please provide valid column names or ",
           "selection helpers.", call. = FALSE)
    }

    # Check that all selected columns are character or factor
    col_types <- vapply(df[col_names], function(x) {
      is.character(x) || is.factor(x)
    }, logical(1))

    if (!all(col_types)) {
      invalid_cols <- col_names[!col_types]
      stop("All selected columns must be character or factor vectors. ",
           "Invalid columns: ", paste(invalid_cols, collapse = ", "),
           call. = FALSE)
    }

    # Get all unique values across all selected columns to create consistent mapping
    all_values <- unique(unlist(lapply(df[col_names], function(x) {
      if (is.factor(x)) as.character(x) else x
    }), use.names = FALSE))
    all_values <- all_values[!is.na(all_values)]  # Remove NAs

    if (length(all_values) == 0) {
      warning("No non-NA values found in selected columns.", call. = FALSE)
      return(df)
    }

    # Create the mapping using mask_labels on all unique values
    mapped_labels <- mask_labels(all_values, prefix = mask_prefix)
    mapping <- stats::setNames(mapped_labels, all_values)

    # Copy data
    result <- df

    # Apply mapping to each selected column using base R
    result[col_names] <- lapply(df[col_names], function(col) {
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

  # Helper to handle one column set (same pattern as scramble_variables_rowwise)
  mask_one_set <- function(set_quo) {
    # Try evaluating as character vector first
    try_char <- tryCatch(
      expr = {
        set <- rlang::eval_tidy(set_quo, data = data)
        if (is.character(set)) {
            if (length(missing) > 0) {
                stop("Error in column selection: Can't subset columns that ",
                     "don't exist. Column `",
                     paste(missing, collapse = "`, `"),
                     "` doesn't exist.", call. = FALSE)
            }
          return(mask_labels_rowwise_internal(data, set, prefix)[set])
        } else {
          NULL
        }
      },
      error = function(e) {
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
      if (is.null(selected) || length(selected) == 0) return(NULL)
      col_names <- names(data)[selected]
      mask_labels_rowwise_internal(data, col_names, prefix)[col_names]
    } else {
      warning("Each column set must be a character vector or ",
              "tidyselect expression.", call. = FALSE)
      return(NULL)
    }
  }

  # Apply to each set
  masked_dfs <- lapply(column_sets, mask_one_set)

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
