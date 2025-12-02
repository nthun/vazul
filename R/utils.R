# Evaluate quosure as character vector
#
# @param set_quo A quosure containing a potential character vector
# @param data A data frame to validate column names against
# @return Character vector of valid column names, or NULL if not a character vector
# @keywords internal
try_evaluate_as_character_names <- function(set_quo, data) {
    tryCatch(
        {
            set <- rlang::eval_tidy(set_quo, data = data)

            if (!is.character(set)) {
                return(NULL)
            }

            missing_cols <- setdiff(set, names(data))
            if (length(missing_cols) > 0) {
                warning("Some column names not found: ",
                        paste(missing_cols, collapse = ", "),
                        call. = FALSE)
                set <- intersect(set, names(data))
                if (length(set) == 0) {
                    return(NULL)
                }
            }

            set
        },
        error = function(e) NULL
    )
}

# Evaluate quosure as tidyselect expression
#
# @param set_quo A quosure containing a tidyselect expression
# @param data A data frame to select columns from
# @return Named vector of column indices, or NULL if evaluation fails
# @keywords internal
evaluate_as_tidyselect_selection <- function(set_quo, data) {
    # Check if it might be numeric positions first
    if (!rlang::quo_is_symbol(set_quo) && !rlang::quo_is_call(set_quo)) {
        positions <- tryCatch(
            {
                pos <- rlang::eval_tidy(set_quo, data = data)
                if (is.numeric(pos)) {
                    pos <- as.integer(pos)
                    valid_pos <- pos[pos > 0 & pos <= ncol(data)]
                    if (length(valid_pos) == 0) {
                        warning("No valid column positions found.", call. = FALSE)
                        return(NULL)
                    }
                    # Return as named vector for consistency
                    result <- valid_pos
                    names(result) <- names(data)[valid_pos]
                    return(result)
                }
                NULL
            },
            error = function(e) NULL
        )

        if (!is.null(positions)) {
            return(positions)
        }

        warning("Each column set must be a character vector, numeric positions, or tidyselect expression.",
                call. = FALSE)
        return(NULL)
    }

    selected <- tryCatch(
        tidyselect::eval_select(set_quo, data),
        error = function(e) {
            warning("Failed to evaluate column set: ", conditionMessage(e),
                    call. = FALSE)
            return(NULL)
        }
    )

    if (is.null(selected) || length(selected) == 0) {
        return(NULL)
    }

    selected
}

# Resolve column indices from multiple column sets
#
# @param column_sets List of quosures representing column selections
# @param data A data frame
# @return Vector of unique column indices, or NULL if none found
# @keywords internal
resolve_column_indices <- function(column_sets, data) {
    process_one_set <- function(set_quo) {
        # Try as character vector first
        char_names <- try_evaluate_as_character_names(set_quo, data)
        if (!is.null(char_names)) {
            return(match(char_names, names(data)))
        }

        # Otherwise, treat as tidyselect expression
        selected <- evaluate_as_tidyselect_selection(set_quo, data)
        if (!is.null(selected)) {
            return(unname(selected))
        }

        NULL
    }

    all_indices <- column_sets |>
        lapply(process_one_set) |>
        Filter(f = Negate(is.null)) |>
        unlist() |>
        unique()

    if (length(all_indices) == 0) {
        warning("No valid columns found. Returning data unchanged.", call. = FALSE)
        return(NULL)
    }

    all_indices
}

# Resolve column names from multiple column sets and apply blinding function
#
# @param column_sets List of quosures representing column selections
# @param data A data frame
# @param blinding_fn The blinding function to apply
# @return List of data frames with blinded columns
# @keywords internal
resolve_and_blind_columns <- function(column_sets, data, blinding_fn) {
    process_one_set <- function(set_quo) {
        # Try as character vector first
        char_names <- try_evaluate_as_character_names(set_quo, data)
        if (!is.null(char_names)) {
            return(blinding_fn(data, dplyr::all_of(char_names))[char_names])
        }

        # Otherwise, treat as tidyselect expression
        selected <- evaluate_as_tidyselect_selection(set_quo, data)
        if (!is.null(selected)) {
            col_names <- names(selected)
            return(blinding_fn(data, dplyr::all_of(col_names))[col_names])
        }

        NULL
    }

    column_sets |>
        lapply(process_one_set) |>
        Filter(f = Negate(is.null))
}

# Merge blinded columns back into original data frame
#
# @param blinded_dfs List of data frames with blinded columns
# @param data Original data frame
# @return Data frame with blinded columns merged in, preserving original column order
# @keywords internal
merge_blinded_columns_into <- function(blinded_dfs, data) {
    if (length(blinded_dfs) == 0) {
        return(data)
    }

    blinded_dfs |>
        Reduce(
            f = function(result, blinded_df) {
                result[names(blinded_df)] <- blinded_df
                result
            },
            init = data
        ) |>
        (\(result) result[names(data)])()
}

# Resolve group column indices
#
# @param groups_quo Quosure for group selection (or NULL)
# @param data A data frame
# @return Vector of group column indices, or NULL
# @keywords internal
resolve_group_indices <- function(groups_quo, data) {
    if (is.null(groups_quo)) {
        return(NULL)
    }

    group_indices <- tryCatch(
        tidyselect::eval_select(rlang::enquo(groups_quo), data),
        error = function(e) {
            warning("Failed to evaluate grouping columns: ", conditionMessage(e),
                    call. = FALSE)
            return(NULL)
        }
    )

    if (is.null(group_indices) || length(group_indices) == 0) {
        warning("No valid grouping columns found. Scrambling without grouping.",
                call. = FALSE)
        return(NULL)
    }

    unname(group_indices)
}
