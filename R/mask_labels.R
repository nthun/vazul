#' Mask categorical labels with random labels
#'
#' Assigns random new labels to each unique value in a character or factor
#' vector. The purpose is to blind data so analysts are not aware of treatment
#' allocation or categorical outcomes. Each unique original value gets a random
#' new label, and the assignment order is randomized to prevent correspondence
#' with the original order.
#' @keywords mask
#' @param x a character or factor vector
#' @param prefix character string to use as prefix for masked labels.
#'   Default is "masked_group_"
#' @return a vector of the same type as input with masked labels
#'
#' @seealso \code{\link{mask_variables}} for masking multiple variables in a data frame,
#' \code{\link{mask_variables_rowwise}} for rowwise masking, and
#' \code{\link{mask_names}} for masking variable names.
#'
#' @examples
#'
#' # Example with character vector
#' set.seed(123)
#' treatment <- c("control", "treatment", "control", "treatment")
#' mask_labels(treatment)
#'
#' # Example with custom prefix
#' set.seed(456)
#' condition <- c("A", "B", "C", "A", "B", "C")
#' mask_labels(condition, prefix = "group_")
#'
#' # Example with factor vector
#' set.seed(789)
#' ecology <- factor(c("Desperate", "Hopeful", "Desperate", "Hopeful"))
#' mask_labels(ecology)
#'
#' # Using with dataset column
#' data(williams)
#' set.seed(123)
#' williams$ecology_masked <- mask_labels(williams$ecology)
#' head(williams[c("ecology", "ecology_masked")])
#'
#' @export
mask_labels <- function(x, prefix = "masked_group_") {
  validate_vector(x)
  validate_vector_categorical(x)
  validate_prefix(prefix)

  # Get unique keys
  char_x <- as.character(x)
  keys <- unique(char_x)

  # Create mapping
  mapping <- create_mapping(keys, prefix = prefix)

  # Warn if all values are NA
  if (length(mapping$keys) == 0) {
    warning("All values in input are NA. Returning unchanged.", call. = FALSE)
    return(x)
  }

  # Apply mapping
  apply_mapping(x, mapping)
}
