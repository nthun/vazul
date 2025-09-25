#' Mask variable names with anonymous labels
#'
#' Assigns new masked names to groups of variables in a data frame.
#' Each group of variables gets its own prefix for the masked names,
#' such as "variable_set_A_01", "variable_set_B_01", etc.
#' The function ensures no name collisions occur with existing column names.
#'
#' @param data a data frame
#' @param ... <tidy-select> One or more variable sets. Each can be:
#'   \itemize{
#'     \item A tidyselect expression (e.g., \code{starts_with("treatment_")})
#'     \item A character vector of column names (e.g., \code{c("var1", "var2")})
#'     \item Multiple sets can be provided as separate arguments
#'   }
#' @param prefix character string to use as base prefix for masked names.
#'   Default is "variable_set_"
#' @param set_id character string to append to prefix for each variable set.
#'   If NULL (default), uses letters A, B, C, etc. for each set.
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
#' # Mask two variable sets with default prefixes
#' mask_names(df,
#'   starts_with("treat_"),
#'   starts_with("outcome_")
#' )
#'
#' # Using character vectors
#' mask_names(df,
#'   c("treat_1", "treat_2"),
#'   c("outcome_a", "outcome_b")
#' )
#'
#' # Custom set_id
#' mask_names(df,
#'   starts_with("treat_"),
#'   starts_with("outcome_"),
#'   set_id = c("treatment", "outcome")
#' )
#'
#' @export
mask_names <- function(data, ..., prefix = "variable_set_", set_id = NULL) {
  stopifnot(is.data.frame(data))

  # Capture all ... arguments as quosures
  column_sets <- rlang::enquos(...)

  # If no sets provided, return data unchanged
  if (length(column_sets) == 0) {
    warning("No variable sets provided. Returning data unchanged.",
            call. = FALSE)
    return(data)
  }

  # Validate prefix parameter
  if (is.null(prefix)) {
    stop("Parameter 'prefix' cannot be NULL. Please provide a ",
         "character string.", call. = FALSE)
  }

  if (!is.character(prefix) || length(prefix) != 1) {
    stop("Parameter 'prefix' must be a single character string.",
         call. = FALSE)
  }

  # Validate set_id parameter if provided
  if (!is.null(set_id)) {
    if (!is.character(set_id)) {
      stop("Parameter 'set_id' must be a character vector or NULL.",
           call. = FALSE)
    }
    if (length(set_id) != length(column_sets)) {
      stop("If 'set_id' is provided, it must have the same length as ",
           "the number of variable sets.", call. = FALSE)
    }
  }

  # Helper function to resolve one column set and generate masked names
  resolve_and_mask_set <- function(set_quo, set_index) {
    # First, resolve the column names from the quosure
    selected_cols <- NULL

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
          set  # Return the character vector
        } else {
          # Not character — return NULL to try tidyselect
          NULL
        }
      },
      error = function(e) {
        # Not a character vector — return NULL to try tidyselect
        NULL
      }
    )

    if (!is.null(try_char)) {
      selected_cols <- try_char
    } else {
      # If not character, treat as tidyselect expression
      if (rlang::quo_is_symbol(set_quo) || rlang::quo_is_call(set_quo)) {
        selected <- tryCatch(
          tidyselect::eval_select(set_quo, data),
          error = function(e) {
            warning("Failed to evaluate variable set: ",
                    conditionMessage(e), call. = FALSE)
            return(integer(0))
          }
        )
        if (length(selected) == 0) {
          return(NULL)
        }
        selected_cols <- names(data)[selected]
      } else {
        warning("Each variable set must be a character vector or ",
                "tidyselect expression.", call. = FALSE)
        return(NULL)
      }
    }

    if (is.null(selected_cols) || length(selected_cols) == 0) {
      return(NULL)
    }

    # Generate set identifier for this set
    if (is.null(set_id)) {
      set_identifier <- LETTERS[set_index]
    } else {
      set_identifier <- set_id[set_index]
    }

    # Create masked names using mask_labels() with set-specific prefix
    set_prefix <- paste0(prefix, set_identifier, "_")
    masked_names <- mask_labels(selected_cols, prefix = set_prefix)

    # Return mapping from original to masked names
    stats::setNames(masked_names, selected_cols)
  }

  # Apply to each set and collect all name mappings
  all_mappings <- Map(resolve_and_mask_set, column_sets, seq_along(column_sets))
  all_mappings <- Filter(Negate(is.null), all_mappings)

  # If no valid sets were processed, return original data
  if (length(all_mappings) == 0) {
    warning("No valid variable sets found. Returning data unchanged.",
            call. = FALSE)
    return(data)
  }

  # Combine all mappings into a single named vector
  final_mapping <- unlist(all_mappings, use.names = TRUE)

  # Check for name collisions with existing column names
  new_names <- as.character(final_mapping)
  existing_names <- setdiff(names(data), names(final_mapping))
  name_collisions <- intersect(new_names, existing_names)

  if (length(name_collisions) > 0) {
    stop("Name collision detected. The following masked names already ",
         "exist in the data: ", paste(name_collisions, collapse = ", "),
         ". Please use different prefixes or suffixes.", call. = FALSE)
  }

  # Check for duplicate masked names across different sets
  if (length(new_names) != length(unique(new_names))) {
    duplicates <- new_names[duplicated(new_names)]
    stop("Duplicate masked names generated: ",
         paste(unique(duplicates), collapse = ", "),
         ". This may occur when different variable sets have the same ",
         "column names. Please use different prefixes or suffixes.",
         call. = FALSE)
  }

  # Apply the name changes to the data frame
  result <- data
  names(result)[match(names(final_mapping), names(result))] <- final_mapping

  return(result)
}
