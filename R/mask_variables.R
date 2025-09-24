#' Mask categorical variables with random labels in a data frame
#'
#' Applies masked labels to multiple categorical variables in a data frame using
#' the \code{mask_labels()} function. Each variable gets independent random
#' masked labels by default, or can optionally use the same masked labels
#' across all selected variables.
#'
#' @param data a data frame
#' @param cols <tidy-select> Columns to mask. Accepts column names, positions,
#'   or tidyselect helpers like \code{starts_with()}, \code{contains()},
#'   \code{where()}, etc. Only character and factor columns will be
#'   processed.
#' @param shared_labels logical. If \code{TRUE}, all selected variables will use
#'   the same set of masked labels. If \code{FALSE} (default), each variable
#'   gets its own independent set of masked labels.
#' @param prefix character string to use as prefix for masked labels.
#'   Default is "masked_group_"
#'
#' @return A data frame with the specified categorical columns masked.
#'   Non-categorical columns in the selection are left unchanged with a
#'   warning.
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
#' # Independent masking for each variable (default)
#' mask_variables(df, c("treatment", "outcome"))
#'
#' set.seed(456)
#' # Shared masking across variables
#' mask_variables(df, c("treatment", "outcome"), shared_labels = TRUE)
#'
#' set.seed(789)
#' # Custom prefix
#' mask_variables(df, c("treatment", "outcome"), prefix = "group_")
#'
#' # Using tidyselect helpers
#' mask_variables(df, where(is.character))
#'
#' # Example with williams dataset
#' data(williams)
#' set.seed(123)
#' williams_masked <- mask_variables(williams, c("ecology", "gender"))
#' head(williams_masked[c("ecology", "gender")])
#'
#' @export
mask_variables <- function(data, cols, shared_labels = FALSE,
                           prefix = "masked_group_") {
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

  # Validate shared_labels parameter
  if (is.null(shared_labels)) {
    stop("Parameter 'shared_labels' cannot be NULL. ",
         "Please provide a logical value.", call. = FALSE)
  }
  
  if (!is.logical(shared_labels) || length(shared_labels) != 1) {
    stop("Parameter 'shared_labels' must be a single logical value ",
         "(TRUE or FALSE).", call. = FALSE)
  }
  
  # Validate prefix parameter (delegate to mask_labels for detailed validation)
  if (is.null(prefix)) {
    stop("Parameter 'prefix' cannot be NULL. ",
         "Please provide a character string.", call. = FALSE)
  }
  
  if (!is.character(prefix) || length(prefix) != 1) {
    stop("Parameter 'prefix' must be a single character string.",
         call. = FALSE)
  }
  
  # Handle column selection using tidyselect
  col_indices <- tryCatch({
    tidyselect::eval_select(rlang::enquo(cols), data)
  }, error = function(e) {
    stop("Error in column selection: ", conditionMessage(e), call. = FALSE)
  })
  
  if (length(col_indices) == 0) {
    warning("No columns selected. Returning original data unchanged.", call. = FALSE)
    return(data)
  }
  
  # Filter to only character and factor columns
  selected_cols <- names(data)[col_indices]
  is_categorical <- vapply(data[selected_cols], function(x) {
    is.character(x) || is.factor(x)
  }, logical(1))
  
  categorical_cols <- selected_cols[is_categorical]
  non_categorical_cols <- selected_cols[!is_categorical]
  
  # Warn about non-categorical columns
  if (length(non_categorical_cols) > 0) {
    warning(
      "The following selected columns are not character or factor and ",
      "will be left unchanged: ",
      paste(non_categorical_cols, collapse = ", "), call. = FALSE
    )
  }
  
  # If no categorical columns, return original data
  if (length(categorical_cols) == 0) {
    warning(
      "No categorical (character or factor) columns found in selection. ",
      "Returning original data unchanged.", call. = FALSE
    )
    return(data)
  }
  
  # Apply masking
  if (shared_labels) {
    # For shared labels, we need to create a unified mapping
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
    
    # Create a single mapping for all unique values
    n_unique <- length(all_values_no_na)
    padding_width <- max(2, nchar(as.character(n_unique)))
    masked_labels <- paste0(
      prefix,
      sprintf(paste0("%0", padding_width, "d"), seq_len(n_unique))
    )
    random_assignment <- sample(masked_labels, n_unique, replace = FALSE)
    shared_mapping <- stats::setNames(random_assignment, all_values_no_na)
    
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
        result <- factor(result, levels = masked_labels)
      }
      
      return(result)
    })
    
  } else {
    # Independent masking for each column
    data[categorical_cols] <- lapply(data[categorical_cols], function(x) {
      if (all(is.na(x))) {
        return(x)  # Return unchanged if all NA
      }
      
      # Apply mask_labels to each column independently
      tryCatch({
        mask_labels(x, prefix = prefix)
      }, error = function(e) {
        warning("Failed to mask column: ", conditionMessage(e),
                call. = FALSE)
        return(x)  # Return original column if masking fails
      })
    })
  }
  
  return(data)
}