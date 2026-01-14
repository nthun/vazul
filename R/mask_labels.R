#' Mask categorical labels with random labels
#'
#' Assigns random new labels to each unique value in a character or factor
#' vector. The purpose is to blind data so analysts are not aware of treatment
#' allocation or categorical outcomes. Each unique original value gets a random
#' new label, and the assignment order is randomized to prevent correspondence
#' with the original order.
#' @keywords functions
#' @param x a character or factor vector
#' @param prefix character string to use as prefix for masked labels.
#'   Default is "masked_group_"
#' @return a vector of the same type as input with masked labels
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
  # Input validation
  validate_vector(x)
  validate_vector_categorical(x)

  # Validate prefix parameter
  validate_prefix(prefix)

  # Get unique values from the input
  unique_values <- unique(x)
  n_unique <- length(unique_values)

  # Create masked labels with numeric padding
  # Determine padding width based on number of unique values
  padding_width <- max(2, nchar(as.character(n_unique)))
  masked_labels <- paste0(prefix, sprintf(paste0("%0", padding_width, "d"), seq_len(n_unique)))

  # Randomly assign masked labels to unique values
  # This ensures the order doesn't correspond to the original order
  random_assignment <- sample(masked_labels, n_unique, replace = FALSE)

  # Create mapping from original values to masked labels
  mapping <- stats::setNames(random_assignment, unique_values)

  # Apply mapping to the original vector
  result <- mapping[as.character(x)]

  # Preserve factor structure if input was a factor
  if (is.factor(x)) {
    result <- factor(result, levels = masked_labels)
  }

  return(result)
}
