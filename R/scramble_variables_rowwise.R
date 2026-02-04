#' Scramble values across multiple columns rowwise in a data frame
#'
#' Internal helper to scramble values rowwise.
#'
#' @param data A data frame.
#' @param cols Character vector of column names to scramble.
#' @return A data frame with values scrambled rowwise within the selected columns.
#' @keywords internal
#' @noRd
scramble_variables_rowwise <- function(data, cols) {
  # Warn if only one column (no scrambling possible)
  if (length(cols) < 2) {
    warning("Only one column selected. Rowwise scrambling requires at least 2 columns. ",
            "Returning original data unchanged.", call. = FALSE)
    return(data)
  }

  # Validate type/class compatibility.
  # Rowwise scrambling assigns values across columns within each row, so columns must be compatible.
  col_classes <- lapply(data[cols], class)
  col_classes_key <- vapply(col_classes, paste, collapse = "|", character(1))
  same_class <- length(unique(col_classes_key)) == 1

  col_types <- vapply(data[cols], typeof, character(1))
  unique_types <- unique(col_types)
  int_double_mix <- length(unique_types) == 2 && setequal(unique_types, c("integer", "double"))

  if (!same_class && !int_double_mix) {
    stop(
      "Rowwise scrambling requires selected columns to have the same class ",
      "(or be an integer/double mix). Selected classes: ",
      paste(unique(vapply(col_classes, paste, collapse = "/", character(1))), collapse = ", "),
      call. = FALSE
    )
  }

  # Extra safety for factors: levels must match, otherwise assignment will introduce NAs.
  if (inherits(data[[cols[[1]]]], "factor")) {
    levs <- lapply(data[cols], levels)
    if (!all(vapply(levs, identical, logical(1), levs[[1]]))) {
      stop(
        "Rowwise scrambling of factors requires identical levels across selected columns.",
        call. = FALSE
      )
    }
  }

  # Copy data
  result <- data

  k <- length(cols)
  n <- nrow(data)

  # Scramble each row by permuting values across the selected columns.
  # This avoids matrix conversion and preserves column classes.
  for (i in seq_len(n)) {
    perm <- scramble_values(seq_len(k))
    row_vals <- lapply(cols, function(nm) data[[nm]][i])
    for (j in seq_len(k)) {
      result[[cols[[j]]]][i] <- row_vals[[perm[[j]]]]
    }
  }

  return(result)
}
