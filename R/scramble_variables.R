#' Scrambling the content of several variables in a data frame
#'
#' Scramble the values of several variables in a data frame.
#'
#' @param data a data frame
#' @param cols a vector of column names or indices to scramble
#' @param .groups a vector of group names to scramble within groups (default is NULL, meaning no grouping)
#'
#' @return A data frame with the specified columns scrambled. If grouping is specified, scrambling is done within each group.
#'
#' @examples
#'
#' df <- data.frame(x = 1:6, y = letters[1:6], group = c("A", "A", "A", "B", "B", "B"))
#'
#' set.seed(123)
#' # Example without grouping. Variables scrambled across the entire data frame.
#' df |> scramble_variables(c("x", "y"))
#'
#' # Example with grouping. Variable only scrambled within groups.
#'
#' df |> scramble_variables("y", .groups = "group")
#'
#' # Example with the 'williams' dataset
#'
#' data(williams)
#' williams |> scramble_variables(c("ecology", "age"))
#'
#' williams |> scramble_variables(1:5)

#' williams |> scramble_variables(c("ecology", "age"), .groups = "gender")
#'
#' # The function is compatible with column indices
#'
#' williams |> scramble_variables(c(1, 2), .groups = c(3))
#'
#' @export

scramble_variables <- function(data, cols, .groups = NULL) {
    # Input validation
    stopifnot(is.data.frame(data))

    # Capture original column order
    orig_order <- names(data)

    # Handle column selection 
    if (is.character(cols)) {
        # Check if column names exist
        missing_cols <- setdiff(cols, names(data))
        if (length(missing_cols) > 0) {
            stop("Some target columns not found in data.", call. = FALSE)
        }
        col_indices <- match(cols, names(data))
    } else if (is.numeric(cols)) {
        # Check if indices are valid
        if (any(cols < 1 | cols > ncol(data))) {
            stop("Some target columns not found in data.", call. = FALSE)
        }
        col_indices <- cols
    } else {
        stop("'cols' must be character vector (column names) or numeric vector (column indices).", call. = FALSE)
    }

    # Handle group selection similarly if provided
    if (!is.null(.groups)) {
        if (is.character(.groups)) {
            missing_groups <- setdiff(.groups, names(data))
            if (length(missing_groups) > 0) {
                stop("Some grouping columns not found in data.", call. = FALSE)
            }
            group_indices <- match(.groups, names(data))
        } else if (is.numeric(.groups)) {
            if (any(.groups < 1 | .groups > ncol(data))) {
                stop("Some grouping columns not found in data.", call. = FALSE)
            }
            group_indices <- .groups
        } else {
            stop("'.groups' must be character vector (column names) or numeric vector (column indices).", call. = FALSE)
        }
    } else {
        group_indices <- NULL
    }

    # Perform scrambling
    if (!is.null(group_indices)) {
        # Get unique group combinations
        group_data <- data[, group_indices, drop = FALSE]
        unique_groups <- unique(group_data)
        
        # Process each group
        result_list <- list()
        for (i in seq_len(nrow(unique_groups))) {
            # Find rows that match this group combination
            group_match <- rep(TRUE, nrow(data))
            for (j in seq_len(ncol(unique_groups))) {
                group_match <- group_match & (data[, group_indices[j]] == unique_groups[i, j])
            }
            group_rows <- which(group_match)
            
            # Scramble the selected columns within this group
            group_subset <- data[group_rows, , drop = FALSE]
            group_subset[, col_indices] <- lapply(group_subset[, col_indices, drop = FALSE], sample)
            result_list[[i]] <- group_subset
        }
        
        # Combine results back together
        data <- do.call(rbind, result_list)
        
        # Restore original row order (approximately)
        row_order <- order(as.numeric(rownames(data)))
        data <- data[row_order, , drop = FALSE]
        rownames(data) <- NULL
    } else {
        # Non-grouped: directly scramble selected columns
        data[, col_indices] <- lapply(data[, col_indices, drop = FALSE], sample)
    }

    data
}
