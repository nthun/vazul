# ──────────────────────────────────────────────────────────────────────────────
# Internal masking helpers
# ──────────────────────────────────────────────────────────────────────────────

#' Create a randomized mapping from original labels to masked labels
#'
#' Internal helper for generating a mapping and the corresponding masked label
#' pool (levels). Uses the same padding rules as mask_labels().
#'
#' @param keys A character vector of unique labels to map. May contain NA.
#' @param prefix A non-empty character scalar used as masked label prefix.
#' @return A list with:
#'   - keys: unique, non-NA character keys
#'   - values: masked labels (same length as keys) in randomized order
#'   - levels: full masked label pool (same length as keys)
#' @keywords internal
#' @noRd
create_mapping <- function(keys, prefix = "masked_group_") {
  # Drop NA from keys
  unique_keys <- keys[!is.na(keys)]
  n_unique <- length(unique_keys)

  # Return empty mapping if no keys
  if (length(n_unique) == 0) {
    return(list(keys = character(0), values = character(0), levels = character(0)))
  }

  padding_width <- max(2, nchar(as.character(n_unique)))
  masked_pool <- paste0(
    prefix,
    sprintf(paste0("%0", padding_width, "d"), seq_len(n_unique))
  )

  # Randomly assign masked labels to keys (does not preserve any original order)
  masked_values <- sample(masked_pool, n_unique, replace = FALSE)

  list(keys = unique_keys, values = masked_values, levels = masked_pool)
}

#' Apply a mapping created by create_mapping() to a vector
#'
#' Internal helper for applying a mapping via match() while preserving factor
#' structure when the input is a factor.
#'
#' @param x A character/factor vector. May include NA and empty strings.
#' @param mapping A mapping list returned by create_mapping().
#' @return A masked vector of the same type as x (character or factor).
#' @keywords internal
#' @noRd
apply_mapping <- function(x, mapping) {
  if (all(is.na(x))) {
    return(x)  # Return unchanged if all NA
  }

  char_x <- as.character(x)
  idx <- match(char_x, mapping$keys)
  result <- mapping$values[idx]

  if (is.factor(x)) {
    result <- factor(result, levels = mapping$levels)
  }

  unname(result)
}

#' Collect unique values from multiple columns
#'
#' Internal helper for gathering all unique non-NA values across multiple
#' columns. Optimized to unique each column before merging.
#'
#' @param data A data frame.
#' @param col_names Character vector of column names to collect from.
#' @return Character vector of unique non-NA values across all columns.
#' @keywords internal
#' @noRd
collect_unique_values <- function(data, col_names) {
  # Unique each column first to reduce intermediate vector size
  all_values <- unlist(
    lapply(data[col_names], function(x) {
      char_x <- if (is.factor(x)) as.character(x) else x
      unique(char_x)
    }), 
    use.names = FALSE
  )
  
  # Final unique on the merged result
  unique(all_values)
}